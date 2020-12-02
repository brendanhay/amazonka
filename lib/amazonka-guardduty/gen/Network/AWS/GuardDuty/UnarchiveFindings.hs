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
-- Module      : Network.AWS.GuardDuty.UnarchiveFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unarchives GuardDuty findings specified by the @findingIds@ .
module Network.AWS.GuardDuty.UnarchiveFindings
  ( -- * Creating a Request
    unarchiveFindings,
    UnarchiveFindings,

    -- * Request Lenses
    uDetectorId,
    uFindingIds,

    -- * Destructuring the Response
    unarchiveFindingsResponse,
    UnarchiveFindingsResponse,

    -- * Response Lenses
    ursResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unarchiveFindings' smart constructor.
data UnarchiveFindings = UnarchiveFindings'
  { _uDetectorId :: !Text,
    _uFindingIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnarchiveFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uDetectorId' - The ID of the detector associated with the findings to unarchive.
--
-- * 'uFindingIds' - The IDs of the findings to unarchive.
unarchiveFindings ::
  -- | 'uDetectorId'
  Text ->
  UnarchiveFindings
unarchiveFindings pDetectorId_ =
  UnarchiveFindings'
    { _uDetectorId = pDetectorId_,
      _uFindingIds = mempty
    }

-- | The ID of the detector associated with the findings to unarchive.
uDetectorId :: Lens' UnarchiveFindings Text
uDetectorId = lens _uDetectorId (\s a -> s {_uDetectorId = a})

-- | The IDs of the findings to unarchive.
uFindingIds :: Lens' UnarchiveFindings [Text]
uFindingIds = lens _uFindingIds (\s a -> s {_uFindingIds = a}) . _Coerce

instance AWSRequest UnarchiveFindings where
  type Rs UnarchiveFindings = UnarchiveFindingsResponse
  request = postJSON guardDuty
  response =
    receiveEmpty
      (\s h x -> UnarchiveFindingsResponse' <$> (pure (fromEnum s)))

instance Hashable UnarchiveFindings

instance NFData UnarchiveFindings

instance ToHeaders UnarchiveFindings where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UnarchiveFindings where
  toJSON UnarchiveFindings' {..} =
    object (catMaybes [Just ("findingIds" .= _uFindingIds)])

instance ToPath UnarchiveFindings where
  toPath UnarchiveFindings' {..} =
    mconcat ["/detector/", toBS _uDetectorId, "/findings/unarchive"]

instance ToQuery UnarchiveFindings where
  toQuery = const mempty

-- | /See:/ 'unarchiveFindingsResponse' smart constructor.
newtype UnarchiveFindingsResponse = UnarchiveFindingsResponse'
  { _ursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnarchiveFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursResponseStatus' - -- | The response status code.
unarchiveFindingsResponse ::
  -- | 'ursResponseStatus'
  Int ->
  UnarchiveFindingsResponse
unarchiveFindingsResponse pResponseStatus_ =
  UnarchiveFindingsResponse' {_ursResponseStatus = pResponseStatus_}

-- | -- | The response status code.
ursResponseStatus :: Lens' UnarchiveFindingsResponse Int
ursResponseStatus = lens _ursResponseStatus (\s a -> s {_ursResponseStatus = a})

instance NFData UnarchiveFindingsResponse
