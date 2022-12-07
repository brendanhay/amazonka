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
-- Module      : Amazonka.Athena.Types.ResultConfigurationUpdates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ResultConfigurationUpdates where

import Amazonka.Athena.Types.AclConfiguration
import Amazonka.Athena.Types.EncryptionConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The information about the updates in the query results, such as output
-- location and encryption configuration for the query results.
--
-- /See:/ 'newResultConfigurationUpdates' smart constructor.
data ResultConfigurationUpdates = ResultConfigurationUpdates'
  { -- | The ACL configuration for the query results.
    aclConfiguration :: Prelude.Maybe AclConfiguration,
    -- | If set to \"true\", indicates that the previously-specified encryption
    -- configuration (also known as the client-side setting) for queries in
    -- this workgroup should be ignored and set to null. If set to \"false\" or
    -- not set, and a value is present in the @EncryptionConfiguration@ in
    -- @ResultConfigurationUpdates@ (the client-side setting), the
    -- @EncryptionConfiguration@ in the workgroup\'s @ResultConfiguration@ will
    -- be updated with the new value. For more information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
    removeEncryptionConfiguration :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services account ID that you expect to be the owner of
    -- the Amazon S3 bucket specified by ResultConfiguration$OutputLocation. If
    -- set, Athena uses the value for @ExpectedBucketOwner@ when it makes
    -- Amazon S3 calls to your specified output location. If the
    -- @ExpectedBucketOwner@ Amazon Web Services account ID does not match the
    -- actual owner of the Amazon S3 bucket, the call fails with a permissions
    -- error.
    --
    -- If workgroup settings override client-side settings, then the query uses
    -- the @ExpectedBucketOwner@ setting that is specified for the workgroup,
    -- and also uses the location for storing query results specified in the
    -- workgroup. See WorkGroupConfiguration$EnforceWorkGroupConfiguration and
    -- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | If set to @true@, indicates that the previously-specified ACL
    -- configuration for queries in this workgroup should be ignored and set to
    -- null. If set to @false@ or not set, and a value is present in the
    -- @AclConfiguration@ of @ResultConfigurationUpdates@, the
    -- @AclConfiguration@ in the workgroup\'s @ResultConfiguration@ is updated
    -- with the new value. For more information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
    removeAclConfiguration :: Prelude.Maybe Prelude.Bool,
    -- | The location in Amazon S3 where your query results are stored, such as
    -- @s3:\/\/path\/to\/query\/bucket\/@. For more information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>
    -- If workgroup settings override client-side settings, then the query uses
    -- the location for the query results and the encryption configuration that
    -- are specified for the workgroup. The \"workgroup settings override\" is
    -- specified in @EnforceWorkGroupConfiguration@ (true\/false) in the
    -- @WorkGroupConfiguration@. See
    -- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
    outputLocation :: Prelude.Maybe Prelude.Text,
    -- | If set to \"true\", removes the Amazon Web Services account ID
    -- previously specified for ResultConfiguration$ExpectedBucketOwner. If set
    -- to \"false\" or not set, and a value is present in the
    -- @ExpectedBucketOwner@ in @ResultConfigurationUpdates@ (the client-side
    -- setting), the @ExpectedBucketOwner@ in the workgroup\'s
    -- @ResultConfiguration@ is updated with the new value. For more
    -- information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
    removeExpectedBucketOwner :: Prelude.Maybe Prelude.Bool,
    -- | If set to \"true\", indicates that the previously-specified query
    -- results location (also known as a client-side setting) for queries in
    -- this workgroup should be ignored and set to null. If set to \"false\" or
    -- not set, and a value is present in the @OutputLocation@ in
    -- @ResultConfigurationUpdates@ (the client-side setting), the
    -- @OutputLocation@ in the workgroup\'s @ResultConfiguration@ will be
    -- updated with the new value. For more information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
    removeOutputLocation :: Prelude.Maybe Prelude.Bool,
    -- | The encryption configuration for the query results.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultConfigurationUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aclConfiguration', 'resultConfigurationUpdates_aclConfiguration' - The ACL configuration for the query results.
--
-- 'removeEncryptionConfiguration', 'resultConfigurationUpdates_removeEncryptionConfiguration' - If set to \"true\", indicates that the previously-specified encryption
-- configuration (also known as the client-side setting) for queries in
-- this workgroup should be ignored and set to null. If set to \"false\" or
-- not set, and a value is present in the @EncryptionConfiguration@ in
-- @ResultConfigurationUpdates@ (the client-side setting), the
-- @EncryptionConfiguration@ in the workgroup\'s @ResultConfiguration@ will
-- be updated with the new value. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
--
-- 'expectedBucketOwner', 'resultConfigurationUpdates_expectedBucketOwner' - The Amazon Web Services account ID that you expect to be the owner of
-- the Amazon S3 bucket specified by ResultConfiguration$OutputLocation. If
-- set, Athena uses the value for @ExpectedBucketOwner@ when it makes
-- Amazon S3 calls to your specified output location. If the
-- @ExpectedBucketOwner@ Amazon Web Services account ID does not match the
-- actual owner of the Amazon S3 bucket, the call fails with a permissions
-- error.
--
-- If workgroup settings override client-side settings, then the query uses
-- the @ExpectedBucketOwner@ setting that is specified for the workgroup,
-- and also uses the location for storing query results specified in the
-- workgroup. See WorkGroupConfiguration$EnforceWorkGroupConfiguration and
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
--
-- 'removeAclConfiguration', 'resultConfigurationUpdates_removeAclConfiguration' - If set to @true@, indicates that the previously-specified ACL
-- configuration for queries in this workgroup should be ignored and set to
-- null. If set to @false@ or not set, and a value is present in the
-- @AclConfiguration@ of @ResultConfigurationUpdates@, the
-- @AclConfiguration@ in the workgroup\'s @ResultConfiguration@ is updated
-- with the new value. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
--
-- 'outputLocation', 'resultConfigurationUpdates_outputLocation' - The location in Amazon S3 where your query results are stored, such as
-- @s3:\/\/path\/to\/query\/bucket\/@. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>
-- If workgroup settings override client-side settings, then the query uses
-- the location for the query results and the encryption configuration that
-- are specified for the workgroup. The \"workgroup settings override\" is
-- specified in @EnforceWorkGroupConfiguration@ (true\/false) in the
-- @WorkGroupConfiguration@. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
--
-- 'removeExpectedBucketOwner', 'resultConfigurationUpdates_removeExpectedBucketOwner' - If set to \"true\", removes the Amazon Web Services account ID
-- previously specified for ResultConfiguration$ExpectedBucketOwner. If set
-- to \"false\" or not set, and a value is present in the
-- @ExpectedBucketOwner@ in @ResultConfigurationUpdates@ (the client-side
-- setting), the @ExpectedBucketOwner@ in the workgroup\'s
-- @ResultConfiguration@ is updated with the new value. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
--
-- 'removeOutputLocation', 'resultConfigurationUpdates_removeOutputLocation' - If set to \"true\", indicates that the previously-specified query
-- results location (also known as a client-side setting) for queries in
-- this workgroup should be ignored and set to null. If set to \"false\" or
-- not set, and a value is present in the @OutputLocation@ in
-- @ResultConfigurationUpdates@ (the client-side setting), the
-- @OutputLocation@ in the workgroup\'s @ResultConfiguration@ will be
-- updated with the new value. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
--
-- 'encryptionConfiguration', 'resultConfigurationUpdates_encryptionConfiguration' - The encryption configuration for the query results.
newResultConfigurationUpdates ::
  ResultConfigurationUpdates
newResultConfigurationUpdates =
  ResultConfigurationUpdates'
    { aclConfiguration =
        Prelude.Nothing,
      removeEncryptionConfiguration = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      removeAclConfiguration = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      removeExpectedBucketOwner = Prelude.Nothing,
      removeOutputLocation = Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing
    }

-- | The ACL configuration for the query results.
resultConfigurationUpdates_aclConfiguration :: Lens.Lens' ResultConfigurationUpdates (Prelude.Maybe AclConfiguration)
resultConfigurationUpdates_aclConfiguration = Lens.lens (\ResultConfigurationUpdates' {aclConfiguration} -> aclConfiguration) (\s@ResultConfigurationUpdates' {} a -> s {aclConfiguration = a} :: ResultConfigurationUpdates)

-- | If set to \"true\", indicates that the previously-specified encryption
-- configuration (also known as the client-side setting) for queries in
-- this workgroup should be ignored and set to null. If set to \"false\" or
-- not set, and a value is present in the @EncryptionConfiguration@ in
-- @ResultConfigurationUpdates@ (the client-side setting), the
-- @EncryptionConfiguration@ in the workgroup\'s @ResultConfiguration@ will
-- be updated with the new value. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
resultConfigurationUpdates_removeEncryptionConfiguration :: Lens.Lens' ResultConfigurationUpdates (Prelude.Maybe Prelude.Bool)
resultConfigurationUpdates_removeEncryptionConfiguration = Lens.lens (\ResultConfigurationUpdates' {removeEncryptionConfiguration} -> removeEncryptionConfiguration) (\s@ResultConfigurationUpdates' {} a -> s {removeEncryptionConfiguration = a} :: ResultConfigurationUpdates)

-- | The Amazon Web Services account ID that you expect to be the owner of
-- the Amazon S3 bucket specified by ResultConfiguration$OutputLocation. If
-- set, Athena uses the value for @ExpectedBucketOwner@ when it makes
-- Amazon S3 calls to your specified output location. If the
-- @ExpectedBucketOwner@ Amazon Web Services account ID does not match the
-- actual owner of the Amazon S3 bucket, the call fails with a permissions
-- error.
--
-- If workgroup settings override client-side settings, then the query uses
-- the @ExpectedBucketOwner@ setting that is specified for the workgroup,
-- and also uses the location for storing query results specified in the
-- workgroup. See WorkGroupConfiguration$EnforceWorkGroupConfiguration and
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
resultConfigurationUpdates_expectedBucketOwner :: Lens.Lens' ResultConfigurationUpdates (Prelude.Maybe Prelude.Text)
resultConfigurationUpdates_expectedBucketOwner = Lens.lens (\ResultConfigurationUpdates' {expectedBucketOwner} -> expectedBucketOwner) (\s@ResultConfigurationUpdates' {} a -> s {expectedBucketOwner = a} :: ResultConfigurationUpdates)

-- | If set to @true@, indicates that the previously-specified ACL
-- configuration for queries in this workgroup should be ignored and set to
-- null. If set to @false@ or not set, and a value is present in the
-- @AclConfiguration@ of @ResultConfigurationUpdates@, the
-- @AclConfiguration@ in the workgroup\'s @ResultConfiguration@ is updated
-- with the new value. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
resultConfigurationUpdates_removeAclConfiguration :: Lens.Lens' ResultConfigurationUpdates (Prelude.Maybe Prelude.Bool)
resultConfigurationUpdates_removeAclConfiguration = Lens.lens (\ResultConfigurationUpdates' {removeAclConfiguration} -> removeAclConfiguration) (\s@ResultConfigurationUpdates' {} a -> s {removeAclConfiguration = a} :: ResultConfigurationUpdates)

-- | The location in Amazon S3 where your query results are stored, such as
-- @s3:\/\/path\/to\/query\/bucket\/@. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>
-- If workgroup settings override client-side settings, then the query uses
-- the location for the query results and the encryption configuration that
-- are specified for the workgroup. The \"workgroup settings override\" is
-- specified in @EnforceWorkGroupConfiguration@ (true\/false) in the
-- @WorkGroupConfiguration@. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
resultConfigurationUpdates_outputLocation :: Lens.Lens' ResultConfigurationUpdates (Prelude.Maybe Prelude.Text)
resultConfigurationUpdates_outputLocation = Lens.lens (\ResultConfigurationUpdates' {outputLocation} -> outputLocation) (\s@ResultConfigurationUpdates' {} a -> s {outputLocation = a} :: ResultConfigurationUpdates)

-- | If set to \"true\", removes the Amazon Web Services account ID
-- previously specified for ResultConfiguration$ExpectedBucketOwner. If set
-- to \"false\" or not set, and a value is present in the
-- @ExpectedBucketOwner@ in @ResultConfigurationUpdates@ (the client-side
-- setting), the @ExpectedBucketOwner@ in the workgroup\'s
-- @ResultConfiguration@ is updated with the new value. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
resultConfigurationUpdates_removeExpectedBucketOwner :: Lens.Lens' ResultConfigurationUpdates (Prelude.Maybe Prelude.Bool)
resultConfigurationUpdates_removeExpectedBucketOwner = Lens.lens (\ResultConfigurationUpdates' {removeExpectedBucketOwner} -> removeExpectedBucketOwner) (\s@ResultConfigurationUpdates' {} a -> s {removeExpectedBucketOwner = a} :: ResultConfigurationUpdates)

-- | If set to \"true\", indicates that the previously-specified query
-- results location (also known as a client-side setting) for queries in
-- this workgroup should be ignored and set to null. If set to \"false\" or
-- not set, and a value is present in the @OutputLocation@ in
-- @ResultConfigurationUpdates@ (the client-side setting), the
-- @OutputLocation@ in the workgroup\'s @ResultConfiguration@ will be
-- updated with the new value. For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
resultConfigurationUpdates_removeOutputLocation :: Lens.Lens' ResultConfigurationUpdates (Prelude.Maybe Prelude.Bool)
resultConfigurationUpdates_removeOutputLocation = Lens.lens (\ResultConfigurationUpdates' {removeOutputLocation} -> removeOutputLocation) (\s@ResultConfigurationUpdates' {} a -> s {removeOutputLocation = a} :: ResultConfigurationUpdates)

-- | The encryption configuration for the query results.
resultConfigurationUpdates_encryptionConfiguration :: Lens.Lens' ResultConfigurationUpdates (Prelude.Maybe EncryptionConfiguration)
resultConfigurationUpdates_encryptionConfiguration = Lens.lens (\ResultConfigurationUpdates' {encryptionConfiguration} -> encryptionConfiguration) (\s@ResultConfigurationUpdates' {} a -> s {encryptionConfiguration = a} :: ResultConfigurationUpdates)

instance Prelude.Hashable ResultConfigurationUpdates where
  hashWithSalt _salt ResultConfigurationUpdates' {..} =
    _salt `Prelude.hashWithSalt` aclConfiguration
      `Prelude.hashWithSalt` removeEncryptionConfiguration
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` removeAclConfiguration
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` removeExpectedBucketOwner
      `Prelude.hashWithSalt` removeOutputLocation
      `Prelude.hashWithSalt` encryptionConfiguration

instance Prelude.NFData ResultConfigurationUpdates where
  rnf ResultConfigurationUpdates' {..} =
    Prelude.rnf aclConfiguration
      `Prelude.seq` Prelude.rnf removeEncryptionConfiguration
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf removeAclConfiguration
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf removeExpectedBucketOwner
      `Prelude.seq` Prelude.rnf removeOutputLocation
      `Prelude.seq` Prelude.rnf encryptionConfiguration

instance Data.ToJSON ResultConfigurationUpdates where
  toJSON ResultConfigurationUpdates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AclConfiguration" Data..=)
              Prelude.<$> aclConfiguration,
            ("RemoveEncryptionConfiguration" Data..=)
              Prelude.<$> removeEncryptionConfiguration,
            ("ExpectedBucketOwner" Data..=)
              Prelude.<$> expectedBucketOwner,
            ("RemoveAclConfiguration" Data..=)
              Prelude.<$> removeAclConfiguration,
            ("OutputLocation" Data..=)
              Prelude.<$> outputLocation,
            ("RemoveExpectedBucketOwner" Data..=)
              Prelude.<$> removeExpectedBucketOwner,
            ("RemoveOutputLocation" Data..=)
              Prelude.<$> removeOutputLocation,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration
          ]
      )
