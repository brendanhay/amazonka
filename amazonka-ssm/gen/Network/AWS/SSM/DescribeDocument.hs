{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeDocument
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified configuration document.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DescribeDocument.html>
module Network.AWS.SSM.DescribeDocument
    (
    -- * Request
      DescribeDocument
    -- ** Request constructor
    , describeDocument
    -- ** Request lenses
    , ddrqName

    -- * Response
    , DescribeDocumentResponse
    -- ** Response constructor
    , describeDocumentResponse
    -- ** Response lenses
    , ddrsDocument
    , ddrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'describeDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrqName'
newtype DescribeDocument = DescribeDocument'
    { _ddrqName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDocument' smart constructor.
describeDocument :: Text -> DescribeDocument
describeDocument pName =
    DescribeDocument'
    { _ddrqName = pName
    }

-- | The name of the configuration document.
ddrqName :: Lens' DescribeDocument Text
ddrqName = lens _ddrqName (\ s a -> s{_ddrqName = a});

instance AWSRequest DescribeDocument where
        type Sv DescribeDocument = SSM
        type Rs DescribeDocument = DescribeDocumentResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDocumentResponse' <$>
                   (x .?> "Document") <*> (pure (fromEnum s)))

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
          = object ["Name" .= _ddrqName]

instance ToPath DescribeDocument where
        toPath = const "/"

instance ToQuery DescribeDocument where
        toQuery = const mempty

-- | /See:/ 'describeDocumentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrsDocument'
--
-- * 'ddrsStatus'
data DescribeDocumentResponse = DescribeDocumentResponse'
    { _ddrsDocument :: !(Maybe DocumentDescription)
    , _ddrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDocumentResponse' smart constructor.
describeDocumentResponse :: Int -> DescribeDocumentResponse
describeDocumentResponse pStatus =
    DescribeDocumentResponse'
    { _ddrsDocument = Nothing
    , _ddrsStatus = pStatus
    }

-- | Information about the configuration document.
ddrsDocument :: Lens' DescribeDocumentResponse (Maybe DocumentDescription)
ddrsDocument = lens _ddrsDocument (\ s a -> s{_ddrsDocument = a});

-- | FIXME: Undocumented member.
ddrsStatus :: Lens' DescribeDocumentResponse Int
ddrsStatus = lens _ddrsStatus (\ s a -> s{_ddrsStatus = a});
