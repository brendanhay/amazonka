{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.PipelineConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.PipelineConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The pipeline configuration for a resolver of kind @PIPELINE@ .
--
--
--
-- /See:/ 'pipelineConfig' smart constructor.
newtype PipelineConfig = PipelineConfig'
  { _pcFunctions ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcFunctions' - A list of @Function@ objects.
pipelineConfig ::
  PipelineConfig
pipelineConfig = PipelineConfig' {_pcFunctions = Nothing}

-- | A list of @Function@ objects.
pcFunctions :: Lens' PipelineConfig [Text]
pcFunctions = lens _pcFunctions (\s a -> s {_pcFunctions = a}) . _Default . _Coerce

instance FromJSON PipelineConfig where
  parseJSON =
    withObject
      "PipelineConfig"
      (\x -> PipelineConfig' <$> (x .:? "functions" .!= mempty))

instance Hashable PipelineConfig

instance NFData PipelineConfig

instance ToJSON PipelineConfig where
  toJSON PipelineConfig' {..} =
    object (catMaybes [("functions" .=) <$> _pcFunctions])
