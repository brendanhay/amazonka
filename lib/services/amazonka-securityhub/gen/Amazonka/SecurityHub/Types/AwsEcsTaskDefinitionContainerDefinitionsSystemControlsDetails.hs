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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A namespaced kernel parameter to set in the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails = AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails'
  { -- | The namespaced kernel parameter for which to set a value.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The value of the parameter.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_namespace' - The namespaced kernel parameter for which to set a value.
--
-- 'value', 'awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_value' - The value of the parameter.
newAwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
newAwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails'
    { namespace =
        Prelude.Nothing,
      value =
        Prelude.Nothing
    }

-- | The namespaced kernel parameter for which to set a value.
awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_namespace :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_namespace = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails' {namespace} -> namespace) (\s@AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails' {} a -> s {namespace = a} :: AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails)

-- | The value of the parameter.
awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_value :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_value = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails' {value} -> value) (\s@AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails' {} a -> s {value = a} :: AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails'
            Prelude.<$> (x Data..:? "Namespace")
              Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails' {..} =
      _salt `Prelude.hashWithSalt` namespace
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails' {..} =
      Prelude.rnf namespace
        `Prelude.seq` Prelude.rnf value

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Namespace" Data..=) Prelude.<$> namespace,
              ("Value" Data..=) Prelude.<$> value
            ]
        )
