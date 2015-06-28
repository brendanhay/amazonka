{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SSM.DescribeDocument
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

-- | Describes the specified configuration document.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DescribeDocument.html>
module Network.AWS.SSM.DescribeDocument
    (
    -- * Request
      DescribeDocument
    -- ** Request constructor
    , describeDocument
    -- ** Request lenses
    , ddName

    -- * Response
    , DescribeDocumentResponse
    -- ** Response constructor
    , describeDocumentResponse
    -- ** Response lenses
    , desDocument
    , desStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'describeDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddName'
newtype DescribeDocument = DescribeDocument'
    { _ddName :: Text
    } deriving (Eq,Read,Show)

-- | 'DescribeDocument' smart constructor.
describeDocument :: Text -> DescribeDocument
describeDocument pName =
    DescribeDocument'
    { _ddName = pName
    }

-- | The name of the configuration document.
ddName :: Lens' DescribeDocument Text
ddName = lens _ddName (\ s a -> s{_ddName = a});

instance AWSRequest DescribeDocument where
        type Sv DescribeDocument = SSM
        type Rs DescribeDocument = DescribeDocumentResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDocumentResponse' <$>
                   (x .?> "Document") <*> (pure s))

instance ToHeaders DescribeDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDocument where
        toJSON DescribeDocument'{..}
          = object ["Name" .= _ddName]

instance ToPath DescribeDocument where
        toPath = const "/"

instance ToQuery DescribeDocument where
        toQuery = const mempty

-- | /See:/ 'describeDocumentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desDocument'
--
-- * 'desStatus'
data DescribeDocumentResponse = DescribeDocumentResponse'
    { _desDocument :: !(Maybe DocumentDescription)
    , _desStatus   :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeDocumentResponse' smart constructor.
describeDocumentResponse :: Status -> DescribeDocumentResponse
describeDocumentResponse pStatus =
    DescribeDocumentResponse'
    { _desDocument = Nothing
    , _desStatus = pStatus
    }

-- | Information about the configuration document.
desDocument :: Lens' DescribeDocumentResponse (Maybe DocumentDescription)
desDocument = lens _desDocument (\ s a -> s{_desDocument = a});

-- | FIXME: Undocumented member.
desStatus :: Lens' DescribeDocumentResponse Status
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});
