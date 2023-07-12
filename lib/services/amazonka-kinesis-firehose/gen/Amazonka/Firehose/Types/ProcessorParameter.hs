{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Firehose.Types.ProcessorParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ProcessorParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.ProcessorParameterName
import qualified Amazonka.Prelude as Prelude

-- | Describes the processor parameter.
--
-- /See:/ 'newProcessorParameter' smart constructor.
data ProcessorParameter = ProcessorParameter'
  { -- | The name of the parameter. Currently the following default values are
    -- supported: 3 for @NumberOfRetries@ and 60 for the
    -- @BufferIntervalInSeconds@. The @BufferSizeInMBs@ ranges between 0.2 MB
    -- and up to 3MB. The default buffering hint is 1MB for all destinations,
    -- except Splunk. For Splunk, the default buffering hint is 256 KB.
    parameterName :: ProcessorParameterName,
    -- | The parameter value.
    parameterValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessorParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterName', 'processorParameter_parameterName' - The name of the parameter. Currently the following default values are
-- supported: 3 for @NumberOfRetries@ and 60 for the
-- @BufferIntervalInSeconds@. The @BufferSizeInMBs@ ranges between 0.2 MB
-- and up to 3MB. The default buffering hint is 1MB for all destinations,
-- except Splunk. For Splunk, the default buffering hint is 256 KB.
--
-- 'parameterValue', 'processorParameter_parameterValue' - The parameter value.
newProcessorParameter ::
  -- | 'parameterName'
  ProcessorParameterName ->
  -- | 'parameterValue'
  Prelude.Text ->
  ProcessorParameter
newProcessorParameter
  pParameterName_
  pParameterValue_ =
    ProcessorParameter'
      { parameterName =
          pParameterName_,
        parameterValue = pParameterValue_
      }

-- | The name of the parameter. Currently the following default values are
-- supported: 3 for @NumberOfRetries@ and 60 for the
-- @BufferIntervalInSeconds@. The @BufferSizeInMBs@ ranges between 0.2 MB
-- and up to 3MB. The default buffering hint is 1MB for all destinations,
-- except Splunk. For Splunk, the default buffering hint is 256 KB.
processorParameter_parameterName :: Lens.Lens' ProcessorParameter ProcessorParameterName
processorParameter_parameterName = Lens.lens (\ProcessorParameter' {parameterName} -> parameterName) (\s@ProcessorParameter' {} a -> s {parameterName = a} :: ProcessorParameter)

-- | The parameter value.
processorParameter_parameterValue :: Lens.Lens' ProcessorParameter Prelude.Text
processorParameter_parameterValue = Lens.lens (\ProcessorParameter' {parameterValue} -> parameterValue) (\s@ProcessorParameter' {} a -> s {parameterValue = a} :: ProcessorParameter)

instance Data.FromJSON ProcessorParameter where
  parseJSON =
    Data.withObject
      "ProcessorParameter"
      ( \x ->
          ProcessorParameter'
            Prelude.<$> (x Data..: "ParameterName")
            Prelude.<*> (x Data..: "ParameterValue")
      )

instance Prelude.Hashable ProcessorParameter where
  hashWithSalt _salt ProcessorParameter' {..} =
    _salt
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` parameterValue

instance Prelude.NFData ProcessorParameter where
  rnf ProcessorParameter' {..} =
    Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf parameterValue

instance Data.ToJSON ProcessorParameter where
  toJSON ProcessorParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ParameterName" Data..= parameterName),
            Prelude.Just
              ("ParameterValue" Data..= parameterValue)
          ]
      )
