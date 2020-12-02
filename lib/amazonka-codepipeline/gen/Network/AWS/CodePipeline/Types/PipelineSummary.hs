{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns a summary of a pipeline.
--
--
--
-- /See:/ 'pipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { _psCreated ::
      !(Maybe POSIX),
    _psName :: !(Maybe Text),
    _psVersion :: !(Maybe Nat),
    _psUpdated :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psCreated' - The date and time the pipeline was created, in timestamp format.
--
-- * 'psName' - The name of the pipeline.
--
-- * 'psVersion' - The version number of the pipeline.
--
-- * 'psUpdated' - The date and time of the last update to the pipeline, in timestamp format.
pipelineSummary ::
  PipelineSummary
pipelineSummary =
  PipelineSummary'
    { _psCreated = Nothing,
      _psName = Nothing,
      _psVersion = Nothing,
      _psUpdated = Nothing
    }

-- | The date and time the pipeline was created, in timestamp format.
psCreated :: Lens' PipelineSummary (Maybe UTCTime)
psCreated = lens _psCreated (\s a -> s {_psCreated = a}) . mapping _Time

-- | The name of the pipeline.
psName :: Lens' PipelineSummary (Maybe Text)
psName = lens _psName (\s a -> s {_psName = a})

-- | The version number of the pipeline.
psVersion :: Lens' PipelineSummary (Maybe Natural)
psVersion = lens _psVersion (\s a -> s {_psVersion = a}) . mapping _Nat

-- | The date and time of the last update to the pipeline, in timestamp format.
psUpdated :: Lens' PipelineSummary (Maybe UTCTime)
psUpdated = lens _psUpdated (\s a -> s {_psUpdated = a}) . mapping _Time

instance FromJSON PipelineSummary where
  parseJSON =
    withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            <$> (x .:? "created")
            <*> (x .:? "name")
            <*> (x .:? "version")
            <*> (x .:? "updated")
      )

instance Hashable PipelineSummary

instance NFData PipelineSummary
