{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about a stage to a job worker.
--
--
--
-- /See:/ 'stageContext' smart constructor.
newtype StageContext = StageContext' {_scName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StageContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scName' - The name of the stage.
stageContext ::
  StageContext
stageContext = StageContext' {_scName = Nothing}

-- | The name of the stage.
scName :: Lens' StageContext (Maybe Text)
scName = lens _scName (\s a -> s {_scName = a})

instance FromJSON StageContext where
  parseJSON =
    withObject
      "StageContext"
      (\x -> StageContext' <$> (x .:? "name"))

instance Hashable StageContext

instance NFData StageContext
