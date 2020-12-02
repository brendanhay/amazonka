{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Mitigation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Mitigation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The mitigation applied to a DDoS attack.
--
--
--
-- /See:/ 'mitigation' smart constructor.
newtype Mitigation = Mitigation' {_mMitigationName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mitigation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMitigationName' - The name of the mitigation taken for this attack.
mitigation ::
  Mitigation
mitigation = Mitigation' {_mMitigationName = Nothing}

-- | The name of the mitigation taken for this attack.
mMitigationName :: Lens' Mitigation (Maybe Text)
mMitigationName = lens _mMitigationName (\s a -> s {_mMitigationName = a})

instance FromJSON Mitigation where
  parseJSON =
    withObject
      "Mitigation"
      (\x -> Mitigation' <$> (x .:? "MitigationName"))

instance Hashable Mitigation

instance NFData Mitigation
