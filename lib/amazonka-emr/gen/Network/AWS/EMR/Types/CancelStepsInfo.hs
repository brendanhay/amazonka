{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.CancelStepsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.CancelStepsInfo where

import Network.AWS.EMR.Types.CancelStepsRequestStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specification of the status of a CancelSteps request. Available only in Amazon EMR version 4.8.0 and later, excluding version 5.0.0.
--
--
--
-- /See:/ 'cancelStepsInfo' smart constructor.
data CancelStepsInfo = CancelStepsInfo'
  { _csiStatus ::
      !(Maybe CancelStepsRequestStatus),
    _csiStepId :: !(Maybe Text),
    _csiReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelStepsInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csiStatus' - The status of a CancelSteps Request. The value may be SUBMITTED or FAILED.
--
-- * 'csiStepId' - The encrypted StepId of a step.
--
-- * 'csiReason' - The reason for the failure if the CancelSteps request fails.
cancelStepsInfo ::
  CancelStepsInfo
cancelStepsInfo =
  CancelStepsInfo'
    { _csiStatus = Nothing,
      _csiStepId = Nothing,
      _csiReason = Nothing
    }

-- | The status of a CancelSteps Request. The value may be SUBMITTED or FAILED.
csiStatus :: Lens' CancelStepsInfo (Maybe CancelStepsRequestStatus)
csiStatus = lens _csiStatus (\s a -> s {_csiStatus = a})

-- | The encrypted StepId of a step.
csiStepId :: Lens' CancelStepsInfo (Maybe Text)
csiStepId = lens _csiStepId (\s a -> s {_csiStepId = a})

-- | The reason for the failure if the CancelSteps request fails.
csiReason :: Lens' CancelStepsInfo (Maybe Text)
csiReason = lens _csiReason (\s a -> s {_csiReason = a})

instance FromJSON CancelStepsInfo where
  parseJSON =
    withObject
      "CancelStepsInfo"
      ( \x ->
          CancelStepsInfo'
            <$> (x .:? "Status") <*> (x .:? "StepId") <*> (x .:? "Reason")
      )

instance Hashable CancelStepsInfo

instance NFData CancelStepsInfo
