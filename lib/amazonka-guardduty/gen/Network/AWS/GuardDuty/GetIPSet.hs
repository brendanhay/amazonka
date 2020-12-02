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
-- Module      : Network.AWS.GuardDuty.GetIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the IPSet specified by the @ipSetId@ .
module Network.AWS.GuardDuty.GetIPSet
  ( -- * Creating a Request
    getIPSet,
    GetIPSet,

    -- * Request Lenses
    gisDetectorId,
    gisIPSetId,

    -- * Destructuring the Response
    getIPSetResponse,
    GetIPSetResponse,

    -- * Response Lenses
    gisrsTags,
    gisrsResponseStatus,
    gisrsName,
    gisrsFormat,
    gisrsLocation,
    gisrsStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIPSet' smart constructor.
data GetIPSet = GetIPSet'
  { _gisDetectorId :: !Text,
    _gisIPSetId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisDetectorId' - The unique ID of the detector that the IPSet is associated with.
--
-- * 'gisIPSetId' - The unique ID of the IPSet to retrieve.
getIPSet ::
  -- | 'gisDetectorId'
  Text ->
  -- | 'gisIPSetId'
  Text ->
  GetIPSet
getIPSet pDetectorId_ pIPSetId_ =
  GetIPSet' {_gisDetectorId = pDetectorId_, _gisIPSetId = pIPSetId_}

-- | The unique ID of the detector that the IPSet is associated with.
gisDetectorId :: Lens' GetIPSet Text
gisDetectorId = lens _gisDetectorId (\s a -> s {_gisDetectorId = a})

-- | The unique ID of the IPSet to retrieve.
gisIPSetId :: Lens' GetIPSet Text
gisIPSetId = lens _gisIPSetId (\s a -> s {_gisIPSetId = a})

instance AWSRequest GetIPSet where
  type Rs GetIPSet = GetIPSetResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          GetIPSetResponse'
            <$> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .:> "name")
            <*> (x .:> "format")
            <*> (x .:> "location")
            <*> (x .:> "status")
      )

instance Hashable GetIPSet

instance NFData GetIPSet

instance ToHeaders GetIPSet where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetIPSet where
  toPath GetIPSet' {..} =
    mconcat
      ["/detector/", toBS _gisDetectorId, "/ipset/", toBS _gisIPSetId]

instance ToQuery GetIPSet where
  toQuery = const mempty

-- | /See:/ 'getIPSetResponse' smart constructor.
data GetIPSetResponse = GetIPSetResponse'
  { _gisrsTags ::
      !(Maybe (Map Text (Text))),
    _gisrsResponseStatus :: !Int,
    _gisrsName :: !Text,
    _gisrsFormat :: !IPSetFormat,
    _gisrsLocation :: !Text,
    _gisrsStatus :: !IPSetStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetIPSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisrsTags' - The tags of the IPSet resource.
--
-- * 'gisrsResponseStatus' - -- | The response status code.
--
-- * 'gisrsName' - The user-friendly name for the IPSet.
--
-- * 'gisrsFormat' - The format of the file that contains the IPSet.
--
-- * 'gisrsLocation' - The URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- * 'gisrsStatus' - The status of IPSet file that was uploaded.
getIPSetResponse ::
  -- | 'gisrsResponseStatus'
  Int ->
  -- | 'gisrsName'
  Text ->
  -- | 'gisrsFormat'
  IPSetFormat ->
  -- | 'gisrsLocation'
  Text ->
  -- | 'gisrsStatus'
  IPSetStatus ->
  GetIPSetResponse
getIPSetResponse
  pResponseStatus_
  pName_
  pFormat_
  pLocation_
  pStatus_ =
    GetIPSetResponse'
      { _gisrsTags = Nothing,
        _gisrsResponseStatus = pResponseStatus_,
        _gisrsName = pName_,
        _gisrsFormat = pFormat_,
        _gisrsLocation = pLocation_,
        _gisrsStatus = pStatus_
      }

-- | The tags of the IPSet resource.
gisrsTags :: Lens' GetIPSetResponse (HashMap Text (Text))
gisrsTags = lens _gisrsTags (\s a -> s {_gisrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gisrsResponseStatus :: Lens' GetIPSetResponse Int
gisrsResponseStatus = lens _gisrsResponseStatus (\s a -> s {_gisrsResponseStatus = a})

-- | The user-friendly name for the IPSet.
gisrsName :: Lens' GetIPSetResponse Text
gisrsName = lens _gisrsName (\s a -> s {_gisrsName = a})

-- | The format of the file that contains the IPSet.
gisrsFormat :: Lens' GetIPSetResponse IPSetFormat
gisrsFormat = lens _gisrsFormat (\s a -> s {_gisrsFormat = a})

-- | The URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
gisrsLocation :: Lens' GetIPSetResponse Text
gisrsLocation = lens _gisrsLocation (\s a -> s {_gisrsLocation = a})

-- | The status of IPSet file that was uploaded.
gisrsStatus :: Lens' GetIPSetResponse IPSetStatus
gisrsStatus = lens _gisrsStatus (\s a -> s {_gisrsStatus = a})

instance NFData GetIPSetResponse
