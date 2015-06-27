{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.DeleteExpression
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Removes an @Expression@ from the search domain. For more information,
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
    , delDomainName
    , delExpressionName

    -- * Response
    , DeleteExpressionResponse
    -- ** Response constructor
    , deleteExpressionResponse
    -- ** Response lenses
    , delExpression
    , delStatus
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
-- * 'delDomainName'
--
-- * 'delExpressionName'
data DeleteExpression = DeleteExpression'
    { _delDomainName     :: Text
    , _delExpressionName :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteExpression' smart constructor.
deleteExpression :: Text -> Text -> DeleteExpression
deleteExpression pDomainName pExpressionName =
    DeleteExpression'
    { _delDomainName = pDomainName
    , _delExpressionName = pExpressionName
    }

-- | FIXME: Undocumented member.
delDomainName :: Lens' DeleteExpression Text
delDomainName = lens _delDomainName (\ s a -> s{_delDomainName = a});

-- | The name of the @Expression@ to delete.
delExpressionName :: Lens' DeleteExpression Text
delExpressionName = lens _delExpressionName (\ s a -> s{_delExpressionName = a});

instance AWSRequest DeleteExpression where
        type Sv DeleteExpression = CloudSearch
        type Rs DeleteExpression = DeleteExpressionResponse
        request = post
        response
          = receiveXMLWrapper "DeleteExpressionResult"
              (\ s h x ->
                 DeleteExpressionResponse' <$>
                   (x .@ "Expression") <*> (pure (fromEnum s)))

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

-- | The result of a @DeleteExpression@ request. Specifies the expression
-- being deleted.
--
-- /See:/ 'deleteExpressionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delExpression'
--
-- * 'delStatus'
data DeleteExpressionResponse = DeleteExpressionResponse'
    { _delExpression :: ExpressionStatus
    , _delStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'DeleteExpressionResponse' smart constructor.
deleteExpressionResponse :: ExpressionStatus -> Int -> DeleteExpressionResponse
deleteExpressionResponse pExpression pStatus =
    DeleteExpressionResponse'
    { _delExpression = pExpression
    , _delStatus = pStatus
    }

-- | The status of the expression being deleted.
delExpression :: Lens' DeleteExpressionResponse ExpressionStatus
delExpression = lens _delExpression (\ s a -> s{_delExpression = a});

-- | FIXME: Undocumented member.
delStatus :: Lens' DeleteExpressionResponse Int
delStatus = lens _delStatus (\ s a -> s{_delStatus = a});
