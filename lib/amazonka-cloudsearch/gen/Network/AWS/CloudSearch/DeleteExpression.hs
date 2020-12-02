{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteExpression
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an @'Expression' @ from the search domain. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DeleteExpression
    (
    -- * Creating a Request
      deleteExpression
    , DeleteExpression
    -- * Request Lenses
    , delDomainName
    , delExpressionName

    -- * Destructuring the Response
    , deleteExpressionResponse
    , DeleteExpressionResponse
    -- * Response Lenses
    , delrsResponseStatus
    , delrsExpression
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DeleteExpression' @ operation. Specifies the name of the domain you want to update and the name of the expression you want to delete.
--
--
--
-- /See:/ 'deleteExpression' smart constructor.
data DeleteExpression = DeleteExpression'
  { _delDomainName     :: !Text
  , _delExpressionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delDomainName' - Undocumented member.
--
-- * 'delExpressionName' - The name of the @'Expression' @ to delete.
deleteExpression
    :: Text -- ^ 'delDomainName'
    -> Text -- ^ 'delExpressionName'
    -> DeleteExpression
deleteExpression pDomainName_ pExpressionName_ =
  DeleteExpression'
    {_delDomainName = pDomainName_, _delExpressionName = pExpressionName_}


-- | Undocumented member.
delDomainName :: Lens' DeleteExpression Text
delDomainName = lens _delDomainName (\ s a -> s{_delDomainName = a})

-- | The name of the @'Expression' @ to delete.
delExpressionName :: Lens' DeleteExpression Text
delExpressionName = lens _delExpressionName (\ s a -> s{_delExpressionName = a})

instance AWSRequest DeleteExpression where
        type Rs DeleteExpression = DeleteExpressionResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DeleteExpressionResult"
              (\ s h x ->
                 DeleteExpressionResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Expression"))

instance Hashable DeleteExpression where

instance NFData DeleteExpression where

instance ToHeaders DeleteExpression where
        toHeaders = const mempty

instance ToPath DeleteExpression where
        toPath = const "/"

instance ToQuery DeleteExpression where
        toQuery DeleteExpression'{..}
          = mconcat
              ["Action" =: ("DeleteExpression" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _delDomainName,
               "ExpressionName" =: _delExpressionName]

-- | The result of a @'DeleteExpression' @ request. Specifies the expression being deleted.
--
--
--
-- /See:/ 'deleteExpressionResponse' smart constructor.
data DeleteExpressionResponse = DeleteExpressionResponse'
  { _delrsResponseStatus :: !Int
  , _delrsExpression     :: !ExpressionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteExpressionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
--
-- * 'delrsExpression' - The status of the expression being deleted.
deleteExpressionResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> ExpressionStatus -- ^ 'delrsExpression'
    -> DeleteExpressionResponse
deleteExpressionResponse pResponseStatus_ pExpression_ =
  DeleteExpressionResponse'
    {_delrsResponseStatus = pResponseStatus_, _delrsExpression = pExpression_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteExpressionResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

-- | The status of the expression being deleted.
delrsExpression :: Lens' DeleteExpressionResponse ExpressionStatus
delrsExpression = lens _delrsExpression (\ s a -> s{_delrsExpression = a})

instance NFData DeleteExpressionResponse where
