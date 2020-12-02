{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.EngineAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.EngineAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A name and value pair that is specific to the engine of the server.
--
--
--
-- /See:/ 'engineAttribute' smart constructor.
data EngineAttribute = EngineAttribute'
  { _eaValue ::
      !(Maybe (Sensitive Text)),
    _eaName :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'EngineAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaValue' - The value of the engine attribute.
--
-- * 'eaName' - The name of the engine attribute.
engineAttribute ::
  EngineAttribute
engineAttribute =
  EngineAttribute' {_eaValue = Nothing, _eaName = Nothing}

-- | The value of the engine attribute.
eaValue :: Lens' EngineAttribute (Maybe Text)
eaValue = lens _eaValue (\s a -> s {_eaValue = a}) . mapping _Sensitive

-- | The name of the engine attribute.
eaName :: Lens' EngineAttribute (Maybe Text)
eaName = lens _eaName (\s a -> s {_eaName = a})

instance FromJSON EngineAttribute where
  parseJSON =
    withObject
      "EngineAttribute"
      (\x -> EngineAttribute' <$> (x .:? "Value") <*> (x .:? "Name"))

instance Hashable EngineAttribute

instance NFData EngineAttribute

instance ToJSON EngineAttribute where
  toJSON EngineAttribute' {..} =
    object
      (catMaybes [("Value" .=) <$> _eaValue, ("Name" .=) <$> _eaName])
