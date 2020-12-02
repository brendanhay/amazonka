{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the settings used to enable point in time recovery.
--
--
--
-- /See:/ 'pointInTimeRecoverySpecification' smart constructor.
newtype PointInTimeRecoverySpecification = PointInTimeRecoverySpecification'
  { _pitrsPointInTimeRecoveryEnabled ::
      Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PointInTimeRecoverySpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pitrsPointInTimeRecoveryEnabled' - Indicates whether point in time recovery is enabled (true) or disabled (false) on the table.
pointInTimeRecoverySpecification ::
  -- | 'pitrsPointInTimeRecoveryEnabled'
  Bool ->
  PointInTimeRecoverySpecification
pointInTimeRecoverySpecification pPointInTimeRecoveryEnabled_ =
  PointInTimeRecoverySpecification'
    { _pitrsPointInTimeRecoveryEnabled =
        pPointInTimeRecoveryEnabled_
    }

-- | Indicates whether point in time recovery is enabled (true) or disabled (false) on the table.
pitrsPointInTimeRecoveryEnabled :: Lens' PointInTimeRecoverySpecification Bool
pitrsPointInTimeRecoveryEnabled = lens _pitrsPointInTimeRecoveryEnabled (\s a -> s {_pitrsPointInTimeRecoveryEnabled = a})

instance Hashable PointInTimeRecoverySpecification

instance NFData PointInTimeRecoverySpecification

instance ToJSON PointInTimeRecoverySpecification where
  toJSON PointInTimeRecoverySpecification' {..} =
    object
      ( catMaybes
          [ Just
              ( "PointInTimeRecoveryEnabled"
                  .= _pitrsPointInTimeRecoveryEnabled
              )
          ]
      )
