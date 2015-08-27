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
-- Module      : Network.AWS.SSM.DescribeDocument
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified configuration document.
--
-- /See:/ <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DescribeDocument.html AWS API Reference> for DescribeDocument.
module Network.AWS.SSM.DescribeDocument
    (
    -- * Creating a Request
      describeDocument
    , DescribeDocument
    -- * Request Lenses
    , ddName

    -- * Destructuring the Response
    , describeDocumentResponse
    , DescribeDocumentResponse
    -- * Response Lenses
    , drsDocument
    , drsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'describeDocument' smart constructor.
newtype DescribeDocument = DescribeDocument'
    { _ddName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddName'
describeDocument
    :: Text -- ^ 'ddName'
    -> DescribeDocument
describeDocument pName_ =
    DescribeDocument'
    { _ddName = pName_
    }

-- | The name of the configuration document.
ddName :: Lens' DescribeDocument Text
ddName = lens _ddName (\ s a -> s{_ddName = a});

instance AWSRequest DescribeDocument where
        type Rs DescribeDocument = DescribeDocumentResponse
        request = postJSON sSM
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
          = object (catMaybes [Just ("Name" .= _ddName)])

instance ToPath DescribeDocument where
        toPath = const "/"

instance ToQuery DescribeDocument where
        toQuery = const mempty

-- | /See:/ 'describeDocumentResponse' smart constructor.
data DescribeDocumentResponse = DescribeDocumentResponse'
    { _drsDocument :: !(Maybe DocumentDescription)
    , _drsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsDocument'
--
-- * 'drsStatus'
describeDocumentResponse
    :: Int -- ^ 'drsStatus'
    -> DescribeDocumentResponse
describeDocumentResponse pStatus_ =
    DescribeDocumentResponse'
    { _drsDocument = Nothing
    , _drsStatus = pStatus_
    }

-- | Information about the configuration document.
drsDocument :: Lens' DescribeDocumentResponse (Maybe DocumentDescription)
drsDocument = lens _drsDocument (\ s a -> s{_drsDocument = a});

-- | The response status code.
drsStatus :: Lens' DescribeDocumentResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
