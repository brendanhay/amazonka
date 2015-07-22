{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteExpression
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes an @Expression@ from the search domain. For more information,
-- see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteExpression.html>
module Network.AWS.CloudSearch.DeleteExpression
    (
    -- * Request
      DeleteExpression
    -- ** Request constructor
    , deleteExpression
    -- ** Request lenses
    , delrqDomainName
    , delrqExpressionName

    -- * Response
    , DeleteExpressionResponse
    -- ** Response constructor
    , deleteExpressionResponse
    -- ** Response lenses
    , delrsStatus
    , delrsExpression
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DeleteExpression@ operation.
-- Specifies the name of the domain you want to update and the name of the
-- expression you want to delete.
--
-- /See:/ 'deleteExpression' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delrqDomainName'
--
-- * 'delrqExpressionName'
data DeleteExpression = DeleteExpression'
    { _delrqDomainName     :: !Text
    , _delrqExpressionName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteExpression' smart constructor.
deleteExpression :: Text -> Text -> DeleteExpression
deleteExpression pDomainName_ pExpressionName_ =
    DeleteExpression'
    { _delrqDomainName = pDomainName_
    , _delrqExpressionName = pExpressionName_
    }

-- | FIXME: Undocumented member.
delrqDomainName :: Lens' DeleteExpression Text
delrqDomainName = lens _delrqDomainName (\ s a -> s{_delrqDomainName = a});

-- | The name of the @Expression@ to delete.
delrqExpressionName :: Lens' DeleteExpression Text
delrqExpressionName = lens _delrqExpressionName (\ s a -> s{_delrqExpressionName = a});

instance AWSRequest DeleteExpression where
        type Sv DeleteExpression = CloudSearch
        type Rs DeleteExpression = DeleteExpressionResponse
        request = post
        response
          = receiveXMLWrapper "DeleteExpressionResult"
              (\ s h x ->
                 DeleteExpressionResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Expression"))

instance ToHeaders DeleteExpression where
        toHeaders = const mempty

instance ToPath DeleteExpression where
        toPath = const "/"

instance ToQuery DeleteExpression where
        toQuery DeleteExpression'{..}
          = mconcat
              ["Action" =: ("DeleteExpression" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _delrqDomainName,
               "ExpressionName" =: _delrqExpressionName]

-- | The result of a @DeleteExpression@ request. Specifies the expression
-- being deleted.
--
-- /See:/ 'deleteExpressionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delrsStatus'
--
-- * 'delrsExpression'
data DeleteExpressionResponse = DeleteExpressionResponse'
    { _delrsStatus     :: !Int
    , _delrsExpression :: !ExpressionStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteExpressionResponse' smart constructor.
deleteExpressionResponse :: Int -> ExpressionStatus -> DeleteExpressionResponse
deleteExpressionResponse pStatus_ pExpression_ =
    DeleteExpressionResponse'
    { _delrsStatus = pStatus_
    , _delrsExpression = pExpression_
    }

-- | FIXME: Undocumented member.
delrsStatus :: Lens' DeleteExpressionResponse Int
delrsStatus = lens _delrsStatus (\ s a -> s{_delrsStatus = a});

-- | The status of the expression being deleted.
delrsExpression :: Lens' DeleteExpressionResponse ExpressionStatus
delrsExpression = lens _delrsExpression (\ s a -> s{_delrsExpression = a});
