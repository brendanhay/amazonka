{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ClassifyDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classification request to analyze a single document in real-time, using a previously created and trained custom model and an endpoint.
module Network.AWS.Comprehend.ClassifyDocument
  ( -- * Creating a Request
    classifyDocument,
    ClassifyDocument,

    -- * Request Lenses
    cdText,
    cdEndpointARN,

    -- * Destructuring the Response
    classifyDocumentResponse,
    ClassifyDocumentResponse,

    -- * Response Lenses
    cdrsLabels,
    cdrsClasses,
    cdrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'classifyDocument' smart constructor.
data ClassifyDocument = ClassifyDocument'
  { _cdText ::
      !(Sensitive Text),
    _cdEndpointARN :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClassifyDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdText' - The document text to be analyzed.
--
-- * 'cdEndpointARN' - The Amazon Resource Number (ARN) of the endpoint.
classifyDocument ::
  -- | 'cdText'
  Text ->
  -- | 'cdEndpointARN'
  Text ->
  ClassifyDocument
classifyDocument pText_ pEndpointARN_ =
  ClassifyDocument'
    { _cdText = _Sensitive # pText_,
      _cdEndpointARN = pEndpointARN_
    }

-- | The document text to be analyzed.
cdText :: Lens' ClassifyDocument Text
cdText = lens _cdText (\s a -> s {_cdText = a}) . _Sensitive

-- | The Amazon Resource Number (ARN) of the endpoint.
cdEndpointARN :: Lens' ClassifyDocument Text
cdEndpointARN = lens _cdEndpointARN (\s a -> s {_cdEndpointARN = a})

instance AWSRequest ClassifyDocument where
  type Rs ClassifyDocument = ClassifyDocumentResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          ClassifyDocumentResponse'
            <$> (x .?> "Labels" .!@ mempty)
            <*> (x .?> "Classes" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ClassifyDocument

instance NFData ClassifyDocument

instance ToHeaders ClassifyDocument where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.ClassifyDocument" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ClassifyDocument where
  toJSON ClassifyDocument' {..} =
    object
      ( catMaybes
          [Just ("Text" .= _cdText), Just ("EndpointArn" .= _cdEndpointARN)]
      )

instance ToPath ClassifyDocument where
  toPath = const "/"

instance ToQuery ClassifyDocument where
  toQuery = const mempty

-- | /See:/ 'classifyDocumentResponse' smart constructor.
data ClassifyDocumentResponse = ClassifyDocumentResponse'
  { _cdrsLabels ::
      !(Maybe [DocumentLabel]),
    _cdrsClasses :: !(Maybe [DocumentClass]),
    _cdrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClassifyDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsLabels' - The labels used the document being analyzed. These are used for multi-label trained models. Individual labels represent different categories that are related in some manner and are not mutually exclusive. For example, a movie can be just an action movie, or it can be an action movie, a science fiction movie, and a comedy, all at the same time.
--
-- * 'cdrsClasses' - The classes used by the document being analyzed. These are used for multi-class trained models. Individual classes are mutually exclusive and each document is expected to have only a single class assigned to it. For example, an animal can be a dog or a cat, but not both at the same time.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
classifyDocumentResponse ::
  -- | 'cdrsResponseStatus'
  Int ->
  ClassifyDocumentResponse
classifyDocumentResponse pResponseStatus_ =
  ClassifyDocumentResponse'
    { _cdrsLabels = Nothing,
      _cdrsClasses = Nothing,
      _cdrsResponseStatus = pResponseStatus_
    }

-- | The labels used the document being analyzed. These are used for multi-label trained models. Individual labels represent different categories that are related in some manner and are not mutually exclusive. For example, a movie can be just an action movie, or it can be an action movie, a science fiction movie, and a comedy, all at the same time.
cdrsLabels :: Lens' ClassifyDocumentResponse [DocumentLabel]
cdrsLabels = lens _cdrsLabels (\s a -> s {_cdrsLabels = a}) . _Default . _Coerce

-- | The classes used by the document being analyzed. These are used for multi-class trained models. Individual classes are mutually exclusive and each document is expected to have only a single class assigned to it. For example, an animal can be a dog or a cat, but not both at the same time.
cdrsClasses :: Lens' ClassifyDocumentResponse [DocumentClass]
cdrsClasses = lens _cdrsClasses (\s a -> s {_cdrsClasses = a}) . _Default . _Coerce

-- | -- | The response status code.
cdrsResponseStatus :: Lens' ClassifyDocumentResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\s a -> s {_cdrsResponseStatus = a})

instance NFData ClassifyDocumentResponse
