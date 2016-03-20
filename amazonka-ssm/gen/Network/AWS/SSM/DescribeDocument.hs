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
-- Describes the specified SSM document.
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
    , drsResponseStatus
    ) where

import           Network.AWS.Lens
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

-- | The name of the SSM document.
ddName :: Lens' DescribeDocument Text
ddName = lens _ddName (\ s a -> s{_ddName = a});

instance AWSRequest DescribeDocument where
        type Rs DescribeDocument = DescribeDocumentResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDocumentResponse' <$>
                   (x .?> "Document") <*> (pure (fromEnum s)))

instance Hashable DescribeDocument

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
    { _drsDocument       :: !(Maybe DocumentDescription)
    , _drsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsDocument'
--
-- * 'drsResponseStatus'
describeDocumentResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeDocumentResponse
describeDocumentResponse pResponseStatus_ =
    DescribeDocumentResponse'
    { _drsDocument = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | Information about the SSM document.
drsDocument :: Lens' DescribeDocumentResponse (Maybe DocumentDescription)
drsDocument = lens _drsDocument (\ s a -> s{_drsDocument = a});

-- | The response status code.
drsResponseStatus :: Lens' DescribeDocumentResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});
