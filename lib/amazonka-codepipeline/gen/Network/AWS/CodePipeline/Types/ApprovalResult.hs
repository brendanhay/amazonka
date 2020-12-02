{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ApprovalResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ApprovalResult where

import Network.AWS.CodePipeline.Types.ApprovalStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the result of an approval request.
--
--
--
-- /See:/ 'approvalResult' smart constructor.
data ApprovalResult = ApprovalResult'
  { _arSummary :: !Text,
    _arStatus :: !ApprovalStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApprovalResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arSummary' - The summary of the current status of the approval request.
--
-- * 'arStatus' - The response submitted by a reviewer assigned to an approval action request.
approvalResult ::
  -- | 'arSummary'
  Text ->
  -- | 'arStatus'
  ApprovalStatus ->
  ApprovalResult
approvalResult pSummary_ pStatus_ =
  ApprovalResult' {_arSummary = pSummary_, _arStatus = pStatus_}

-- | The summary of the current status of the approval request.
arSummary :: Lens' ApprovalResult Text
arSummary = lens _arSummary (\s a -> s {_arSummary = a})

-- | The response submitted by a reviewer assigned to an approval action request.
arStatus :: Lens' ApprovalResult ApprovalStatus
arStatus = lens _arStatus (\s a -> s {_arStatus = a})

instance Hashable ApprovalResult

instance NFData ApprovalResult

instance ToJSON ApprovalResult where
  toJSON ApprovalResult' {..} =
    object
      ( catMaybes
          [Just ("summary" .= _arSummary), Just ("status" .= _arStatus)]
      )
