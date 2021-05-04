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
-- Module      : Network.AWS.Athena.Types.ResultConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ResultConfiguration where

import Network.AWS.Athena.Types.EncryptionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The location in Amazon S3 where query results are stored and the
-- encryption option, if any, used for query results. These are known as
-- \"client-side settings\". If workgroup settings override client-side
-- settings, then the query uses the workgroup settings.
--
-- /See:/ 'newResultConfiguration' smart constructor.
data ResultConfiguration = ResultConfiguration'
  { -- | If query results are encrypted in Amazon S3, indicates the encryption
    -- option used (for example, @SSE-KMS@ or @CSE-KMS@) and key information.
    -- This is a client-side setting. If workgroup settings override
    -- client-side settings, then the query uses the encryption configuration
    -- that is specified for the workgroup, and also uses the location for
    -- storing query results specified in the workgroup. See
    -- WorkGroupConfiguration$EnforceWorkGroupConfiguration and
    -- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The location in Amazon S3 where your query results are stored, such as
    -- @s3:\/\/path\/to\/query\/bucket\/@. To run the query, you must specify
    -- the query results location using one of the ways: either for individual
    -- queries using either this setting (client-side), or in the workgroup,
    -- using WorkGroupConfiguration. If none of them is set, Athena issues an
    -- error that no output location is provided. For more information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>.
    -- If workgroup settings override client-side settings, then the query uses
    -- the settings specified for the workgroup. See
    -- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
    outputLocation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResultConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfiguration', 'resultConfiguration_encryptionConfiguration' - If query results are encrypted in Amazon S3, indicates the encryption
-- option used (for example, @SSE-KMS@ or @CSE-KMS@) and key information.
-- This is a client-side setting. If workgroup settings override
-- client-side settings, then the query uses the encryption configuration
-- that is specified for the workgroup, and also uses the location for
-- storing query results specified in the workgroup. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration and
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
--
-- 'outputLocation', 'resultConfiguration_outputLocation' - The location in Amazon S3 where your query results are stored, such as
-- @s3:\/\/path\/to\/query\/bucket\/@. To run the query, you must specify
-- the query results location using one of the ways: either for individual
-- queries using either this setting (client-side), or in the workgroup,
-- using WorkGroupConfiguration. If none of them is set, Athena issues an
-- error that no output location is provided. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>.
-- If workgroup settings override client-side settings, then the query uses
-- the settings specified for the workgroup. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
newResultConfiguration ::
  ResultConfiguration
newResultConfiguration =
  ResultConfiguration'
    { encryptionConfiguration =
        Prelude.Nothing,
      outputLocation = Prelude.Nothing
    }

-- | If query results are encrypted in Amazon S3, indicates the encryption
-- option used (for example, @SSE-KMS@ or @CSE-KMS@) and key information.
-- This is a client-side setting. If workgroup settings override
-- client-side settings, then the query uses the encryption configuration
-- that is specified for the workgroup, and also uses the location for
-- storing query results specified in the workgroup. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration and
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
resultConfiguration_encryptionConfiguration :: Lens.Lens' ResultConfiguration (Prelude.Maybe EncryptionConfiguration)
resultConfiguration_encryptionConfiguration = Lens.lens (\ResultConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@ResultConfiguration' {} a -> s {encryptionConfiguration = a} :: ResultConfiguration)

-- | The location in Amazon S3 where your query results are stored, such as
-- @s3:\/\/path\/to\/query\/bucket\/@. To run the query, you must specify
-- the query results location using one of the ways: either for individual
-- queries using either this setting (client-side), or in the workgroup,
-- using WorkGroupConfiguration. If none of them is set, Athena issues an
-- error that no output location is provided. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>.
-- If workgroup settings override client-side settings, then the query uses
-- the settings specified for the workgroup. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
resultConfiguration_outputLocation :: Lens.Lens' ResultConfiguration (Prelude.Maybe Prelude.Text)
resultConfiguration_outputLocation = Lens.lens (\ResultConfiguration' {outputLocation} -> outputLocation) (\s@ResultConfiguration' {} a -> s {outputLocation = a} :: ResultConfiguration)

instance Prelude.FromJSON ResultConfiguration where
  parseJSON =
    Prelude.withObject
      "ResultConfiguration"
      ( \x ->
          ResultConfiguration'
            Prelude.<$> (x Prelude..:? "EncryptionConfiguration")
            Prelude.<*> (x Prelude..:? "OutputLocation")
      )

instance Prelude.Hashable ResultConfiguration

instance Prelude.NFData ResultConfiguration

instance Prelude.ToJSON ResultConfiguration where
  toJSON ResultConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EncryptionConfiguration" Prelude..=)
              Prelude.<$> encryptionConfiguration,
            ("OutputLocation" Prelude..=)
              Prelude.<$> outputLocation
          ]
      )
