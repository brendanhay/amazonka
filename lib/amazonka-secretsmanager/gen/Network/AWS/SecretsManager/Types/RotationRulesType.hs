{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.RotationRulesType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.RotationRulesType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that defines the rotation configuration for the secret.
--
--
--
-- /See:/ 'rotationRulesType' smart constructor.
newtype RotationRulesType = RotationRulesType'
  { _rrtAutomaticallyAfterDays ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RotationRulesType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrtAutomaticallyAfterDays' - Specifies the number of days between automatic scheduled rotations of the secret. Secrets Manager schedules the next rotation when the previous one is complete. Secrets Manager schedules the date by adding the rotation interval (number of days) to the actual date of the last rotation. The service chooses the hour within that 24-hour date window randomly. The minute is also chosen somewhat randomly, but weighted towards the top of the hour and influenced by a variety of factors that help distribute load.
rotationRulesType ::
  RotationRulesType
rotationRulesType =
  RotationRulesType' {_rrtAutomaticallyAfterDays = Nothing}

-- | Specifies the number of days between automatic scheduled rotations of the secret. Secrets Manager schedules the next rotation when the previous one is complete. Secrets Manager schedules the date by adding the rotation interval (number of days) to the actual date of the last rotation. The service chooses the hour within that 24-hour date window randomly. The minute is also chosen somewhat randomly, but weighted towards the top of the hour and influenced by a variety of factors that help distribute load.
rrtAutomaticallyAfterDays :: Lens' RotationRulesType (Maybe Natural)
rrtAutomaticallyAfterDays = lens _rrtAutomaticallyAfterDays (\s a -> s {_rrtAutomaticallyAfterDays = a}) . mapping _Nat

instance FromJSON RotationRulesType where
  parseJSON =
    withObject
      "RotationRulesType"
      (\x -> RotationRulesType' <$> (x .:? "AutomaticallyAfterDays"))

instance Hashable RotationRulesType

instance NFData RotationRulesType

instance ToJSON RotationRulesType where
  toJSON RotationRulesType' {..} =
    object
      ( catMaybes
          [("AutomaticallyAfterDays" .=) <$> _rrtAutomaticallyAfterDays]
      )
