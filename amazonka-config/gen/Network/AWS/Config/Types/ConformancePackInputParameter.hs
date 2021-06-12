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
-- Module      : Network.AWS.Config.Types.ConformancePackInputParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackInputParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Input parameters in the form of key-value pairs for the conformance
-- pack, both of which you define. Keys can have a maximum character length
-- of 255 characters, and values can have a maximum length of 4096
-- characters.
--
-- /See:/ 'newConformancePackInputParameter' smart constructor.
data ConformancePackInputParameter = ConformancePackInputParameter'
  { -- | One part of a key-value pair.
    parameterName :: Core.Text,
    -- | Another part of the key-value pair.
    parameterValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConformancePackInputParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterName', 'conformancePackInputParameter_parameterName' - One part of a key-value pair.
--
-- 'parameterValue', 'conformancePackInputParameter_parameterValue' - Another part of the key-value pair.
newConformancePackInputParameter ::
  -- | 'parameterName'
  Core.Text ->
  -- | 'parameterValue'
  Core.Text ->
  ConformancePackInputParameter
newConformancePackInputParameter
  pParameterName_
  pParameterValue_ =
    ConformancePackInputParameter'
      { parameterName =
          pParameterName_,
        parameterValue = pParameterValue_
      }

-- | One part of a key-value pair.
conformancePackInputParameter_parameterName :: Lens.Lens' ConformancePackInputParameter Core.Text
conformancePackInputParameter_parameterName = Lens.lens (\ConformancePackInputParameter' {parameterName} -> parameterName) (\s@ConformancePackInputParameter' {} a -> s {parameterName = a} :: ConformancePackInputParameter)

-- | Another part of the key-value pair.
conformancePackInputParameter_parameterValue :: Lens.Lens' ConformancePackInputParameter Core.Text
conformancePackInputParameter_parameterValue = Lens.lens (\ConformancePackInputParameter' {parameterValue} -> parameterValue) (\s@ConformancePackInputParameter' {} a -> s {parameterValue = a} :: ConformancePackInputParameter)

instance Core.FromJSON ConformancePackInputParameter where
  parseJSON =
    Core.withObject
      "ConformancePackInputParameter"
      ( \x ->
          ConformancePackInputParameter'
            Core.<$> (x Core..: "ParameterName")
            Core.<*> (x Core..: "ParameterValue")
      )

instance Core.Hashable ConformancePackInputParameter

instance Core.NFData ConformancePackInputParameter

instance Core.ToJSON ConformancePackInputParameter where
  toJSON ConformancePackInputParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ParameterName" Core..= parameterName),
            Core.Just ("ParameterValue" Core..= parameterValue)
          ]
      )
