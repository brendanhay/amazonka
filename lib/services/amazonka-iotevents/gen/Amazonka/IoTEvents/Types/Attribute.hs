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
-- Module      : Amazonka.IoTEvents.Types.Attribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.Attribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The attributes from the JSON payload that are made available by the
-- input. Inputs are derived from messages sent to the AWS IoT Events
-- system using @BatchPutMessage@. Each such message contains a JSON
-- payload. Those attributes (and their paired values) specified here are
-- available for use in the @condition@ expressions used by detectors.
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { -- | An expression that specifies an attribute-value pair in a JSON
    -- structure. Use this to specify an attribute from the JSON payload that
    -- is made available by the input. Inputs are derived from messages sent to
    -- AWS IoT Events (@BatchPutMessage@). Each such message contains a JSON
    -- payload. The attribute (and its paired value) specified here are
    -- available for use in the @condition@ expressions used by detectors.
    --
    -- Syntax: @\<field-name>.\<field-name>...@
    jsonPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jsonPath', 'attribute_jsonPath' - An expression that specifies an attribute-value pair in a JSON
-- structure. Use this to specify an attribute from the JSON payload that
-- is made available by the input. Inputs are derived from messages sent to
-- AWS IoT Events (@BatchPutMessage@). Each such message contains a JSON
-- payload. The attribute (and its paired value) specified here are
-- available for use in the @condition@ expressions used by detectors.
--
-- Syntax: @\<field-name>.\<field-name>...@
newAttribute ::
  -- | 'jsonPath'
  Prelude.Text ->
  Attribute
newAttribute pJsonPath_ =
  Attribute' {jsonPath = pJsonPath_}

-- | An expression that specifies an attribute-value pair in a JSON
-- structure. Use this to specify an attribute from the JSON payload that
-- is made available by the input. Inputs are derived from messages sent to
-- AWS IoT Events (@BatchPutMessage@). Each such message contains a JSON
-- payload. The attribute (and its paired value) specified here are
-- available for use in the @condition@ expressions used by detectors.
--
-- Syntax: @\<field-name>.\<field-name>...@
attribute_jsonPath :: Lens.Lens' Attribute Prelude.Text
attribute_jsonPath = Lens.lens (\Attribute' {jsonPath} -> jsonPath) (\s@Attribute' {} a -> s {jsonPath = a} :: Attribute)

instance Data.FromJSON Attribute where
  parseJSON =
    Data.withObject
      "Attribute"
      ( \x ->
          Attribute' Prelude.<$> (x Data..: "jsonPath")
      )

instance Prelude.Hashable Attribute where
  hashWithSalt _salt Attribute' {..} =
    _salt `Prelude.hashWithSalt` jsonPath

instance Prelude.NFData Attribute where
  rnf Attribute' {..} = Prelude.rnf jsonPath

instance Data.ToJSON Attribute where
  toJSON Attribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("jsonPath" Data..= jsonPath)]
      )
