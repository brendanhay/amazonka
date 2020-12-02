{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata where

import Network.AWS.CodeCommit.Types.ApprovalState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a change in the approval state for a pull request.
--
--
--
-- /See:/ 'approvalStateChangedEventMetadata' smart constructor.
data ApprovalStateChangedEventMetadata = ApprovalStateChangedEventMetadata'
  { _ascemApprovalStatus ::
      !(Maybe ApprovalState),
    _ascemRevisionId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApprovalStateChangedEventMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascemApprovalStatus' - The approval status for the pull request.
--
-- * 'ascemRevisionId' - The revision ID of the pull request when the approval state changed.
approvalStateChangedEventMetadata ::
  ApprovalStateChangedEventMetadata
approvalStateChangedEventMetadata =
  ApprovalStateChangedEventMetadata'
    { _ascemApprovalStatus =
        Nothing,
      _ascemRevisionId = Nothing
    }

-- | The approval status for the pull request.
ascemApprovalStatus :: Lens' ApprovalStateChangedEventMetadata (Maybe ApprovalState)
ascemApprovalStatus = lens _ascemApprovalStatus (\s a -> s {_ascemApprovalStatus = a})

-- | The revision ID of the pull request when the approval state changed.
ascemRevisionId :: Lens' ApprovalStateChangedEventMetadata (Maybe Text)
ascemRevisionId = lens _ascemRevisionId (\s a -> s {_ascemRevisionId = a})

instance FromJSON ApprovalStateChangedEventMetadata where
  parseJSON =
    withObject
      "ApprovalStateChangedEventMetadata"
      ( \x ->
          ApprovalStateChangedEventMetadata'
            <$> (x .:? "approvalStatus") <*> (x .:? "revisionId")
      )

instance Hashable ApprovalStateChangedEventMetadata

instance NFData ApprovalStateChangedEventMetadata
