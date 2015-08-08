{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineExpression
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Configures an @Expression@ for the search domain. Used to create new
-- expressions and modify existing ones. If the expression exists, the new
-- configuration replaces the old one. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineExpression.html AWS API Reference> for DefineExpression.
module Network.AWS.CloudSearch.DefineExpression
    (
    -- * Creating a Request
      DefineExpression
    , defineExpression
    -- * Request Lenses
    , dDomainName
    , dExpression

    -- * Destructuring the Response
    , DefineExpressionResponse
    , defineExpressionResponse
    -- * Response Lenses
    , dersStatus
    , dersExpression
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DefineExpression@ operation.
-- Specifies the name of the domain you want to update and the expression
-- you want to configure.
--
-- /See:/ 'defineExpression' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dDomainName'
--
-- * 'dExpression'
data DefineExpression = DefineExpression'
    { _dDomainName :: !Text
    , _dExpression :: !Expression
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineExpression' smart constructor.
defineExpression :: Text -> Expression -> DefineExpression
defineExpression pDomainName_ pExpression_ =
    DefineExpression'
    { _dDomainName = pDomainName_
    , _dExpression = pExpression_
    }

-- | Undocumented member.
dDomainName :: Lens' DefineExpression Text
dDomainName = lens _dDomainName (\ s a -> s{_dDomainName = a});

-- | Undocumented member.
dExpression :: Lens' DefineExpression Expression
dExpression = lens _dExpression (\ s a -> s{_dExpression = a});

instance AWSRequest DefineExpression where
        type Sv DefineExpression = CloudSearch
        type Rs DefineExpression = DefineExpressionResponse
        request = postQuery
        response
          = receiveXMLWrapper "DefineExpressionResult"
              (\ s h x ->
                 DefineExpressionResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Expression"))

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

-- | The result of a @DefineExpression@ request. Contains the status of the
-- newly-configured expression.
--
-- /See:/ 'defineExpressionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dersStatus'
--
-- * 'dersExpression'
data DefineExpressionResponse = DefineExpressionResponse'
    { _dersStatus     :: !Int
    , _dersExpression :: !ExpressionStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineExpressionResponse' smart constructor.
defineExpressionResponse :: Int -> ExpressionStatus -> DefineExpressionResponse
defineExpressionResponse pStatus_ pExpression_ =
    DefineExpressionResponse'
    { _dersStatus = pStatus_
    , _dersExpression = pExpression_
    }

-- | Undocumented member.
dersStatus :: Lens' DefineExpressionResponse Int
dersStatus = lens _dersStatus (\ s a -> s{_dersStatus = a});

-- | Undocumented member.
dersExpression :: Lens' DefineExpressionResponse ExpressionStatus
dersExpression = lens _dersExpression (\ s a -> s{_dersExpression = a});
