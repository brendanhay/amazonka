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
-- Module      : Amazonka.AppMesh.Types.AwsCloudMapInstanceAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.AwsCloudMapInstanceAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the Cloud Map attribute information for your
-- virtual node.
--
-- Cloud Map is not available in the eu-south-1 Region.
--
-- /See:/ 'newAwsCloudMapInstanceAttribute' smart constructor.
data AwsCloudMapInstanceAttribute = AwsCloudMapInstanceAttribute'
  { -- | The name of an Cloud Map service instance attribute key. Any Cloud Map
    -- service instance that contains the specified key and value is returned.
    key :: Prelude.Text,
    -- | The value of an Cloud Map service instance attribute key. Any Cloud Map
    -- service instance that contains the specified key and value is returned.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudMapInstanceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'awsCloudMapInstanceAttribute_key' - The name of an Cloud Map service instance attribute key. Any Cloud Map
-- service instance that contains the specified key and value is returned.
--
-- 'value', 'awsCloudMapInstanceAttribute_value' - The value of an Cloud Map service instance attribute key. Any Cloud Map
-- service instance that contains the specified key and value is returned.
newAwsCloudMapInstanceAttribute ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  AwsCloudMapInstanceAttribute
newAwsCloudMapInstanceAttribute pKey_ pValue_ =
  AwsCloudMapInstanceAttribute'
    { key = pKey_,
      value = pValue_
    }

-- | The name of an Cloud Map service instance attribute key. Any Cloud Map
-- service instance that contains the specified key and value is returned.
awsCloudMapInstanceAttribute_key :: Lens.Lens' AwsCloudMapInstanceAttribute Prelude.Text
awsCloudMapInstanceAttribute_key = Lens.lens (\AwsCloudMapInstanceAttribute' {key} -> key) (\s@AwsCloudMapInstanceAttribute' {} a -> s {key = a} :: AwsCloudMapInstanceAttribute)

-- | The value of an Cloud Map service instance attribute key. Any Cloud Map
-- service instance that contains the specified key and value is returned.
awsCloudMapInstanceAttribute_value :: Lens.Lens' AwsCloudMapInstanceAttribute Prelude.Text
awsCloudMapInstanceAttribute_value = Lens.lens (\AwsCloudMapInstanceAttribute' {value} -> value) (\s@AwsCloudMapInstanceAttribute' {} a -> s {value = a} :: AwsCloudMapInstanceAttribute)

instance Core.FromJSON AwsCloudMapInstanceAttribute where
  parseJSON =
    Core.withObject
      "AwsCloudMapInstanceAttribute"
      ( \x ->
          AwsCloudMapInstanceAttribute'
            Prelude.<$> (x Core..: "key") Prelude.<*> (x Core..: "value")
      )

instance
  Prelude.Hashable
    AwsCloudMapInstanceAttribute
  where
  hashWithSalt _salt AwsCloudMapInstanceAttribute' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData AwsCloudMapInstanceAttribute where
  rnf AwsCloudMapInstanceAttribute' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Core.ToJSON AwsCloudMapInstanceAttribute where
  toJSON AwsCloudMapInstanceAttribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Core..= key),
            Prelude.Just ("value" Core..= value)
          ]
      )
