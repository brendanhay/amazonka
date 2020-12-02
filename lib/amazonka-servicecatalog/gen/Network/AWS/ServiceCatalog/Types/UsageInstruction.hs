{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.UsageInstruction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UsageInstruction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Additional information provided by the administrator.
--
--
--
-- /See:/ 'usageInstruction' smart constructor.
data UsageInstruction = UsageInstruction'
  { _uiValue ::
      !(Maybe Text),
    _uiType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsageInstruction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiValue' - The usage instruction value for this type.
--
-- * 'uiType' - The usage instruction type for the value.
usageInstruction ::
  UsageInstruction
usageInstruction =
  UsageInstruction' {_uiValue = Nothing, _uiType = Nothing}

-- | The usage instruction value for this type.
uiValue :: Lens' UsageInstruction (Maybe Text)
uiValue = lens _uiValue (\s a -> s {_uiValue = a})

-- | The usage instruction type for the value.
uiType :: Lens' UsageInstruction (Maybe Text)
uiType = lens _uiType (\s a -> s {_uiType = a})

instance FromJSON UsageInstruction where
  parseJSON =
    withObject
      "UsageInstruction"
      (\x -> UsageInstruction' <$> (x .:? "Value") <*> (x .:? "Type"))

instance Hashable UsageInstruction

instance NFData UsageInstruction
