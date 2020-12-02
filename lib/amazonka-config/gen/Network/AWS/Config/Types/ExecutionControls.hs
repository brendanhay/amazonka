{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ExecutionControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ExecutionControls where

import Network.AWS.Config.Types.SsmControls
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The controls that AWS Config uses for executing remediations.
--
--
--
-- /See:/ 'executionControls' smart constructor.
newtype ExecutionControls = ExecutionControls'
  { _ecSsmControls ::
      Maybe SsmControls
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionControls' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecSsmControls' - A SsmControls object.
executionControls ::
  ExecutionControls
executionControls = ExecutionControls' {_ecSsmControls = Nothing}

-- | A SsmControls object.
ecSsmControls :: Lens' ExecutionControls (Maybe SsmControls)
ecSsmControls = lens _ecSsmControls (\s a -> s {_ecSsmControls = a})

instance FromJSON ExecutionControls where
  parseJSON =
    withObject
      "ExecutionControls"
      (\x -> ExecutionControls' <$> (x .:? "SsmControls"))

instance Hashable ExecutionControls

instance NFData ExecutionControls

instance ToJSON ExecutionControls where
  toJSON ExecutionControls' {..} =
    object (catMaybes [("SsmControls" .=) <$> _ecSsmControls])
