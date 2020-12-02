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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Systems Manager document.
--
--
module Network.AWS.SSM.DescribeDocument
    (
    -- * Creating a Request
      describeDocument
    , DescribeDocument
    -- * Request Lenses
    , ddDocumentVersion
    , ddName

    -- * Destructuring the Response
    , describeDocumentResponse
    , DescribeDocumentResponse
    -- * Response Lenses
    , desrsDocument
    , desrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeDocument' smart constructor.
data DescribeDocument = DescribeDocument'
  { _ddDocumentVersion :: !(Maybe Text)
  , _ddName            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDocumentVersion' - The document version for which you want information. Can be a specific version or the default version.
--
-- * 'ddName' - The name of the Systems Manager document.
describeDocument
    :: Text -- ^ 'ddName'
    -> DescribeDocument
describeDocument pName_ =
  DescribeDocument' {_ddDocumentVersion = Nothing, _ddName = pName_}


-- | The document version for which you want information. Can be a specific version or the default version.
ddDocumentVersion :: Lens' DescribeDocument (Maybe Text)
ddDocumentVersion = lens _ddDocumentVersion (\ s a -> s{_ddDocumentVersion = a})

-- | The name of the Systems Manager document.
ddName :: Lens' DescribeDocument Text
ddName = lens _ddName (\ s a -> s{_ddName = a})

instance AWSRequest DescribeDocument where
        type Rs DescribeDocument = DescribeDocumentResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDocumentResponse' <$>
                   (x .?> "Document") <*> (pure (fromEnum s)))

instance Hashable DescribeDocument where

instance NFData DescribeDocument where

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
          = object
              (catMaybes
                 [("DocumentVersion" .=) <$> _ddDocumentVersion,
                  Just ("Name" .= _ddName)])

instance ToPath DescribeDocument where
        toPath = const "/"

instance ToQuery DescribeDocument where
        toQuery = const mempty

-- | /See:/ 'describeDocumentResponse' smart constructor.
data DescribeDocumentResponse = DescribeDocumentResponse'
  { _desrsDocument       :: !(Maybe DocumentDescription)
  , _desrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsDocument' - Information about the Systems Manager document.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeDocumentResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeDocumentResponse
describeDocumentResponse pResponseStatus_ =
  DescribeDocumentResponse'
    {_desrsDocument = Nothing, _desrsResponseStatus = pResponseStatus_}


-- | Information about the Systems Manager document.
desrsDocument :: Lens' DescribeDocumentResponse (Maybe DocumentDescription)
desrsDocument = lens _desrsDocument (\ s a -> s{_desrsDocument = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeDocumentResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeDocumentResponse where
