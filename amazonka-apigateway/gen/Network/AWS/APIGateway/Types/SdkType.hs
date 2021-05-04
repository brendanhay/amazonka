{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.Types.SdkType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.SdkType where

import Network.AWS.APIGateway.Types.SdkConfigurationProperty
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A type of SDK that API Gateway can generate.
--
-- /See:/ 'newSdkType' smart constructor.
data SdkType = SdkType'
  { -- | The user-friendly name of an SdkType instance.
    friendlyName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an SdkType instance.
    id :: Prelude.Maybe Prelude.Text,
    -- | A list of configuration properties of an SdkType.
    configurationProperties :: Prelude.Maybe [SdkConfigurationProperty],
    -- | The description of an SdkType.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SdkType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'friendlyName', 'sdkType_friendlyName' - The user-friendly name of an SdkType instance.
--
-- 'id', 'sdkType_id' - The identifier of an SdkType instance.
--
-- 'configurationProperties', 'sdkType_configurationProperties' - A list of configuration properties of an SdkType.
--
-- 'description', 'sdkType_description' - The description of an SdkType.
newSdkType ::
  SdkType
newSdkType =
  SdkType'
    { friendlyName = Prelude.Nothing,
      id = Prelude.Nothing,
      configurationProperties = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The user-friendly name of an SdkType instance.
sdkType_friendlyName :: Lens.Lens' SdkType (Prelude.Maybe Prelude.Text)
sdkType_friendlyName = Lens.lens (\SdkType' {friendlyName} -> friendlyName) (\s@SdkType' {} a -> s {friendlyName = a} :: SdkType)

-- | The identifier of an SdkType instance.
sdkType_id :: Lens.Lens' SdkType (Prelude.Maybe Prelude.Text)
sdkType_id = Lens.lens (\SdkType' {id} -> id) (\s@SdkType' {} a -> s {id = a} :: SdkType)

-- | A list of configuration properties of an SdkType.
sdkType_configurationProperties :: Lens.Lens' SdkType (Prelude.Maybe [SdkConfigurationProperty])
sdkType_configurationProperties = Lens.lens (\SdkType' {configurationProperties} -> configurationProperties) (\s@SdkType' {} a -> s {configurationProperties = a} :: SdkType) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of an SdkType.
sdkType_description :: Lens.Lens' SdkType (Prelude.Maybe Prelude.Text)
sdkType_description = Lens.lens (\SdkType' {description} -> description) (\s@SdkType' {} a -> s {description = a} :: SdkType)

instance Prelude.FromJSON SdkType where
  parseJSON =
    Prelude.withObject
      "SdkType"
      ( \x ->
          SdkType'
            Prelude.<$> (x Prelude..:? "friendlyName")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> ( x Prelude..:? "configurationProperties"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "description")
      )

instance Prelude.Hashable SdkType

instance Prelude.NFData SdkType
