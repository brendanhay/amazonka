{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetError where

import Network.AWS.AppStream.Types.FleetErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a fleet error.
--
--
--
-- /See:/ 'fleetError' smart constructor.
data FleetError = FleetError'
  { _feErrorCode ::
      !(Maybe FleetErrorCode),
    _feErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'feErrorCode' - The error code.
--
-- * 'feErrorMessage' - The error message.
fleetError ::
  FleetError
fleetError =
  FleetError' {_feErrorCode = Nothing, _feErrorMessage = Nothing}

-- | The error code.
feErrorCode :: Lens' FleetError (Maybe FleetErrorCode)
feErrorCode = lens _feErrorCode (\s a -> s {_feErrorCode = a})

-- | The error message.
feErrorMessage :: Lens' FleetError (Maybe Text)
feErrorMessage = lens _feErrorMessage (\s a -> s {_feErrorMessage = a})

instance FromJSON FleetError where
  parseJSON =
    withObject
      "FleetError"
      ( \x ->
          FleetError' <$> (x .:? "ErrorCode") <*> (x .:? "ErrorMessage")
      )

instance Hashable FleetError

instance NFData FleetError
