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
-- Module      : Network.AWS.CloudSearch.DefineExpression
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an @'Expression' @ for the search domain. Used to create new expressions and modify existing ones. If the expression exists, the new configuration replaces the old one. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DefineExpression
    (
    -- * Creating a Request
      defineExpression
    , DefineExpression
    -- * Request Lenses
    , dDomainName
    , dExpression

    -- * Destructuring the Response
    , defineExpressionResponse
    , DefineExpressionResponse
    -- * Response Lenses
    , dersResponseStatus
    , dersExpression
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DefineExpression' @ operation. Specifies the name of the domain you want to update and the expression you want to configure.
--
--
--
-- /See:/ 'defineExpression' smart constructor.
data DefineExpression = DefineExpression'
  { _dDomainName :: !Text
  , _dExpression :: !Expression
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefineExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDomainName' - Undocumented member.
--
-- * 'dExpression' - Undocumented member.
defineExpression
    :: Text -- ^ 'dDomainName'
    -> Expression -- ^ 'dExpression'
    -> DefineExpression
defineExpression pDomainName_ pExpression_ =
  DefineExpression' {_dDomainName = pDomainName_, _dExpression = pExpression_}


-- | Undocumented member.
dDomainName :: Lens' DefineExpression Text
dDomainName = lens _dDomainName (\ s a -> s{_dDomainName = a})

-- | Undocumented member.
dExpression :: Lens' DefineExpression Expression
dExpression = lens _dExpression (\ s a -> s{_dExpression = a})

instance AWSRequest DefineExpression where
        type Rs DefineExpression = DefineExpressionResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DefineExpressionResult"
              (\ s h x ->
                 DefineExpressionResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Expression"))

instance Hashable DefineExpression where

instance NFData DefineExpression where

instance ToHeaders DefineExpression where
        toHeaders = const mempty

instance ToPath DefineExpression where
        toPath = const "/"

instance ToQuery DefineExpression where
        toQuery DefineExpression'{..}
          = mconcat
              ["Action" =: ("DefineExpression" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dDomainName,
               "Expression" =: _dExpression]

-- | The result of a @DefineExpression@ request. Contains the status of the newly-configured expression.
--
--
--
-- /See:/ 'defineExpressionResponse' smart constructor.
data DefineExpressionResponse = DefineExpressionResponse'
  { _dersResponseStatus :: !Int
  , _dersExpression     :: !ExpressionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefineExpressionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersResponseStatus' - -- | The response status code.
--
-- * 'dersExpression' - Undocumented member.
defineExpressionResponse
    :: Int -- ^ 'dersResponseStatus'
    -> ExpressionStatus -- ^ 'dersExpression'
    -> DefineExpressionResponse
defineExpressionResponse pResponseStatus_ pExpression_ =
  DefineExpressionResponse'
    {_dersResponseStatus = pResponseStatus_, _dersExpression = pExpression_}


-- | -- | The response status code.
dersResponseStatus :: Lens' DefineExpressionResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

-- | Undocumented member.
dersExpression :: Lens' DefineExpressionResponse ExpressionStatus
dersExpression = lens _dersExpression (\ s a -> s{_dersExpression = a})

instance NFData DefineExpressionResponse where
