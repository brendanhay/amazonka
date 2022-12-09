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
-- Module      : Amazonka.APIGateway.Types.SdkType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.SdkType where

import Amazonka.APIGateway.Types.SdkConfigurationProperty
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A type of SDK that API Gateway can generate.
--
-- /See:/ 'newSdkType' smart constructor.
data SdkType = SdkType'
  { -- | A list of configuration properties of an SdkType.
    configurationProperties :: Prelude.Maybe [SdkConfigurationProperty],
    -- | The description of an SdkType.
    description :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of an SdkType instance.
    friendlyName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an SdkType instance.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SdkType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationProperties', 'sdkType_configurationProperties' - A list of configuration properties of an SdkType.
--
-- 'description', 'sdkType_description' - The description of an SdkType.
--
-- 'friendlyName', 'sdkType_friendlyName' - The user-friendly name of an SdkType instance.
--
-- 'id', 'sdkType_id' - The identifier of an SdkType instance.
newSdkType ::
  SdkType
newSdkType =
  SdkType'
    { configurationProperties = Prelude.Nothing,
      description = Prelude.Nothing,
      friendlyName = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | A list of configuration properties of an SdkType.
sdkType_configurationProperties :: Lens.Lens' SdkType (Prelude.Maybe [SdkConfigurationProperty])
sdkType_configurationProperties = Lens.lens (\SdkType' {configurationProperties} -> configurationProperties) (\s@SdkType' {} a -> s {configurationProperties = a} :: SdkType) Prelude.. Lens.mapping Lens.coerced

-- | The description of an SdkType.
sdkType_description :: Lens.Lens' SdkType (Prelude.Maybe Prelude.Text)
sdkType_description = Lens.lens (\SdkType' {description} -> description) (\s@SdkType' {} a -> s {description = a} :: SdkType)

-- | The user-friendly name of an SdkType instance.
sdkType_friendlyName :: Lens.Lens' SdkType (Prelude.Maybe Prelude.Text)
sdkType_friendlyName = Lens.lens (\SdkType' {friendlyName} -> friendlyName) (\s@SdkType' {} a -> s {friendlyName = a} :: SdkType)

-- | The identifier of an SdkType instance.
sdkType_id :: Lens.Lens' SdkType (Prelude.Maybe Prelude.Text)
sdkType_id = Lens.lens (\SdkType' {id} -> id) (\s@SdkType' {} a -> s {id = a} :: SdkType)

instance Data.FromJSON SdkType where
  parseJSON =
    Data.withObject
      "SdkType"
      ( \x ->
          SdkType'
            Prelude.<$> ( x Data..:? "configurationProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "friendlyName")
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable SdkType where
  hashWithSalt _salt SdkType' {..} =
    _salt
      `Prelude.hashWithSalt` configurationProperties
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` friendlyName
      `Prelude.hashWithSalt` id

instance Prelude.NFData SdkType where
  rnf SdkType' {..} =
    Prelude.rnf configurationProperties
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf friendlyName
      `Prelude.seq` Prelude.rnf id
