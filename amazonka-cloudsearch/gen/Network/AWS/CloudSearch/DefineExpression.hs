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
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineExpression.html>
module Network.AWS.CloudSearch.DefineExpression
    (
    -- * Request
      DefineExpression
    -- ** Request constructor
    , defineExpression
    -- ** Request lenses
    , defDomainName
    , defExpression

    -- * Response
    , DefineExpressionResponse
    -- ** Response constructor
    , defineExpressionResponse
    -- ** Response lenses
    , derStatus
    , derExpression
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
-- * 'defDomainName'
--
-- * 'defExpression'
data DefineExpression = DefineExpression'
    { _defDomainName :: !Text
    , _defExpression :: !Expression
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineExpression' smart constructor.
defineExpression :: Text -> Expression -> DefineExpression
defineExpression pDomainName pExpression =
    DefineExpression'
    { _defDomainName = pDomainName
    , _defExpression = pExpression
    }

-- | FIXME: Undocumented member.
defDomainName :: Lens' DefineExpression Text
defDomainName = lens _defDomainName (\ s a -> s{_defDomainName = a});

-- | FIXME: Undocumented member.
defExpression :: Lens' DefineExpression Expression
defExpression = lens _defExpression (\ s a -> s{_defExpression = a});

instance AWSRequest DefineExpression where
        type Sv DefineExpression = CloudSearch
        type Rs DefineExpression = DefineExpressionResponse
        request = post
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
               "DomainName" =: _defDomainName,
               "Expression" =: _defExpression]

-- | The result of a @DefineExpression@ request. Contains the status of the
-- newly-configured expression.
--
-- /See:/ 'defineExpressionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derStatus'
--
-- * 'derExpression'
data DefineExpressionResponse = DefineExpressionResponse'
    { _derStatus     :: !Int
    , _derExpression :: !ExpressionStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineExpressionResponse' smart constructor.
defineExpressionResponse :: Int -> ExpressionStatus -> DefineExpressionResponse
defineExpressionResponse pStatus pExpression =
    DefineExpressionResponse'
    { _derStatus = pStatus
    , _derExpression = pExpression
    }

-- | FIXME: Undocumented member.
derStatus :: Lens' DefineExpressionResponse Int
derStatus = lens _derStatus (\ s a -> s{_derStatus = a});

-- | FIXME: Undocumented member.
derExpression :: Lens' DefineExpressionResponse ExpressionStatus
derExpression = lens _derExpression (\ s a -> s{_derExpression = a});
