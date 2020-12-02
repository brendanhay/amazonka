{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a WorkSpace that could not be rebooted. ('RebootWorkspaces' ), rebuilt ('RebuildWorkspaces' ), restored ('RestoreWorkspace' ), terminated ('TerminateWorkspaces' ), started ('StartWorkspaces' ), or stopped ('StopWorkspaces' ).
--
--
--
-- /See:/ 'failedWorkspaceChangeRequest' smart constructor.
data FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest'
  { _fwcrErrorCode ::
      !(Maybe Text),
    _fwcrWorkspaceId :: !(Maybe Text),
    _fwcrErrorMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailedWorkspaceChangeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fwcrErrorCode' - The error code that is returned if the WorkSpace cannot be rebooted.
--
-- * 'fwcrWorkspaceId' - The identifier of the WorkSpace.
--
-- * 'fwcrErrorMessage' - The text of the error message that is returned if the WorkSpace cannot be rebooted.
failedWorkspaceChangeRequest ::
  FailedWorkspaceChangeRequest
failedWorkspaceChangeRequest =
  FailedWorkspaceChangeRequest'
    { _fwcrErrorCode = Nothing,
      _fwcrWorkspaceId = Nothing,
      _fwcrErrorMessage = Nothing
    }

-- | The error code that is returned if the WorkSpace cannot be rebooted.
fwcrErrorCode :: Lens' FailedWorkspaceChangeRequest (Maybe Text)
fwcrErrorCode = lens _fwcrErrorCode (\s a -> s {_fwcrErrorCode = a})

-- | The identifier of the WorkSpace.
fwcrWorkspaceId :: Lens' FailedWorkspaceChangeRequest (Maybe Text)
fwcrWorkspaceId = lens _fwcrWorkspaceId (\s a -> s {_fwcrWorkspaceId = a})

-- | The text of the error message that is returned if the WorkSpace cannot be rebooted.
fwcrErrorMessage :: Lens' FailedWorkspaceChangeRequest (Maybe Text)
fwcrErrorMessage = lens _fwcrErrorMessage (\s a -> s {_fwcrErrorMessage = a})

instance FromJSON FailedWorkspaceChangeRequest where
  parseJSON =
    withObject
      "FailedWorkspaceChangeRequest"
      ( \x ->
          FailedWorkspaceChangeRequest'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "WorkspaceId")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable FailedWorkspaceChangeRequest

instance NFData FailedWorkspaceChangeRequest
