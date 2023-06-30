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
-- Module      : Amazonka.LakeFormation.Types.DataLakeSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.DataLakeSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.DataLakePrincipal
import Amazonka.LakeFormation.Types.PrincipalPermissions
import qualified Amazonka.Prelude as Prelude

-- | A structure representing a list of Lake Formation principals designated
-- as data lake administrators and lists of principal permission entries
-- for default create database and default create table permissions.
--
-- /See:/ 'newDataLakeSettings' smart constructor.
data DataLakeSettings = DataLakeSettings'
  { -- | Whether to allow Amazon EMR clusters to access data managed by Lake
    -- Formation.
    --
    -- If true, you allow Amazon EMR clusters to access data in Amazon S3
    -- locations that are registered with Lake Formation.
    --
    -- If false or null, no Amazon EMR clusters will be able to access data in
    -- Amazon S3 locations that are registered with Lake Formation.
    --
    -- For more information, see
    -- <https://docs-aws.amazon.com/lake-formation/latest/dg/getting-started-setup.html#emr-switch (Optional) Allow Data Filtering on Amazon EMR>.
    allowExternalDataFiltering :: Prelude.Maybe Prelude.Bool,
    -- | Lake Formation relies on a privileged process secured by Amazon EMR or
    -- the third party integrator to tag the user\'s role while assuming it.
    -- Lake Formation will publish the acceptable key-value pair, for example
    -- key = \"LakeFormationTrustedCaller\" and value = \"TRUE\" and the third
    -- party integrator must properly tag the temporary security credentials
    -- that will be used to call Lake Formation\'s administrative APIs.
    authorizedSessionTagValueList :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether access control on newly created database is managed by
    -- Lake Formation permissions or exclusively by IAM permissions.
    --
    -- A null value indicates access control by Lake Formation permissions. A
    -- value that assigns ALL to IAM_ALLOWED_PRINCIPALS indicates access
    -- control by IAM permissions. This is referred to as the setting \"Use
    -- only IAM access control,\" and is for backward compatibility with the
    -- Glue permission model implemented by IAM permissions.
    --
    -- The only permitted values are an empty array or an array that contains a
    -- single JSON object that grants ALL to IAM_ALLOWED_PRINCIPALS.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lake-formation/latest/dg/change-settings.html Changing the Default Security Settings for Your Data Lake>.
    createDatabaseDefaultPermissions :: Prelude.Maybe [PrincipalPermissions],
    -- | Specifies whether access control on newly created table is managed by
    -- Lake Formation permissions or exclusively by IAM permissions.
    --
    -- A null value indicates access control by Lake Formation permissions. A
    -- value that assigns ALL to IAM_ALLOWED_PRINCIPALS indicates access
    -- control by IAM permissions. This is referred to as the setting \"Use
    -- only IAM access control,\" and is for backward compatibility with the
    -- Glue permission model implemented by IAM permissions.
    --
    -- The only permitted values are an empty array or an array that contains a
    -- single JSON object that grants ALL to IAM_ALLOWED_PRINCIPALS.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lake-formation/latest/dg/change-settings.html Changing the Default Security Settings for Your Data Lake>.
    createTableDefaultPermissions :: Prelude.Maybe [PrincipalPermissions],
    -- | A list of Lake Formation principals. Supported principals are IAM users
    -- or IAM roles.
    dataLakeAdmins :: Prelude.Maybe [DataLakePrincipal],
    -- | A list of the account IDs of Amazon Web Services accounts with Amazon
    -- EMR clusters that are to perform data filtering.>
    externalDataFilteringAllowList :: Prelude.Maybe [DataLakePrincipal],
    -- | A key-value map that provides an additional configuration on your data
    -- lake. CrossAccountVersion is the key you can configure in the Parameters
    -- field. Accepted values for the CrossAccountVersion key are 1, 2, and 3.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of the resource-owning account IDs that the caller\'s account can
    -- use to share their user access details (user ARNs). The user ARNs can be
    -- logged in the resource owner\'s CloudTrail log.
    --
    -- You may want to specify this property when you are in a high-trust
    -- boundary, such as the same team or company.
    trustedResourceOwners :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowExternalDataFiltering', 'dataLakeSettings_allowExternalDataFiltering' - Whether to allow Amazon EMR clusters to access data managed by Lake
-- Formation.
--
-- If true, you allow Amazon EMR clusters to access data in Amazon S3
-- locations that are registered with Lake Formation.
--
-- If false or null, no Amazon EMR clusters will be able to access data in
-- Amazon S3 locations that are registered with Lake Formation.
--
-- For more information, see
-- <https://docs-aws.amazon.com/lake-formation/latest/dg/getting-started-setup.html#emr-switch (Optional) Allow Data Filtering on Amazon EMR>.
--
-- 'authorizedSessionTagValueList', 'dataLakeSettings_authorizedSessionTagValueList' - Lake Formation relies on a privileged process secured by Amazon EMR or
-- the third party integrator to tag the user\'s role while assuming it.
-- Lake Formation will publish the acceptable key-value pair, for example
-- key = \"LakeFormationTrustedCaller\" and value = \"TRUE\" and the third
-- party integrator must properly tag the temporary security credentials
-- that will be used to call Lake Formation\'s administrative APIs.
--
-- 'createDatabaseDefaultPermissions', 'dataLakeSettings_createDatabaseDefaultPermissions' - Specifies whether access control on newly created database is managed by
-- Lake Formation permissions or exclusively by IAM permissions.
--
-- A null value indicates access control by Lake Formation permissions. A
-- value that assigns ALL to IAM_ALLOWED_PRINCIPALS indicates access
-- control by IAM permissions. This is referred to as the setting \"Use
-- only IAM access control,\" and is for backward compatibility with the
-- Glue permission model implemented by IAM permissions.
--
-- The only permitted values are an empty array or an array that contains a
-- single JSON object that grants ALL to IAM_ALLOWED_PRINCIPALS.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lake-formation/latest/dg/change-settings.html Changing the Default Security Settings for Your Data Lake>.
--
-- 'createTableDefaultPermissions', 'dataLakeSettings_createTableDefaultPermissions' - Specifies whether access control on newly created table is managed by
-- Lake Formation permissions or exclusively by IAM permissions.
--
-- A null value indicates access control by Lake Formation permissions. A
-- value that assigns ALL to IAM_ALLOWED_PRINCIPALS indicates access
-- control by IAM permissions. This is referred to as the setting \"Use
-- only IAM access control,\" and is for backward compatibility with the
-- Glue permission model implemented by IAM permissions.
--
-- The only permitted values are an empty array or an array that contains a
-- single JSON object that grants ALL to IAM_ALLOWED_PRINCIPALS.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lake-formation/latest/dg/change-settings.html Changing the Default Security Settings for Your Data Lake>.
--
-- 'dataLakeAdmins', 'dataLakeSettings_dataLakeAdmins' - A list of Lake Formation principals. Supported principals are IAM users
-- or IAM roles.
--
-- 'externalDataFilteringAllowList', 'dataLakeSettings_externalDataFilteringAllowList' - A list of the account IDs of Amazon Web Services accounts with Amazon
-- EMR clusters that are to perform data filtering.>
--
-- 'parameters', 'dataLakeSettings_parameters' - A key-value map that provides an additional configuration on your data
-- lake. CrossAccountVersion is the key you can configure in the Parameters
-- field. Accepted values for the CrossAccountVersion key are 1, 2, and 3.
--
-- 'trustedResourceOwners', 'dataLakeSettings_trustedResourceOwners' - A list of the resource-owning account IDs that the caller\'s account can
-- use to share their user access details (user ARNs). The user ARNs can be
-- logged in the resource owner\'s CloudTrail log.
--
-- You may want to specify this property when you are in a high-trust
-- boundary, such as the same team or company.
newDataLakeSettings ::
  DataLakeSettings
newDataLakeSettings =
  DataLakeSettings'
    { allowExternalDataFiltering =
        Prelude.Nothing,
      authorizedSessionTagValueList = Prelude.Nothing,
      createDatabaseDefaultPermissions = Prelude.Nothing,
      createTableDefaultPermissions = Prelude.Nothing,
      dataLakeAdmins = Prelude.Nothing,
      externalDataFilteringAllowList = Prelude.Nothing,
      parameters = Prelude.Nothing,
      trustedResourceOwners = Prelude.Nothing
    }

-- | Whether to allow Amazon EMR clusters to access data managed by Lake
-- Formation.
--
-- If true, you allow Amazon EMR clusters to access data in Amazon S3
-- locations that are registered with Lake Formation.
--
-- If false or null, no Amazon EMR clusters will be able to access data in
-- Amazon S3 locations that are registered with Lake Formation.
--
-- For more information, see
-- <https://docs-aws.amazon.com/lake-formation/latest/dg/getting-started-setup.html#emr-switch (Optional) Allow Data Filtering on Amazon EMR>.
dataLakeSettings_allowExternalDataFiltering :: Lens.Lens' DataLakeSettings (Prelude.Maybe Prelude.Bool)
dataLakeSettings_allowExternalDataFiltering = Lens.lens (\DataLakeSettings' {allowExternalDataFiltering} -> allowExternalDataFiltering) (\s@DataLakeSettings' {} a -> s {allowExternalDataFiltering = a} :: DataLakeSettings)

-- | Lake Formation relies on a privileged process secured by Amazon EMR or
-- the third party integrator to tag the user\'s role while assuming it.
-- Lake Formation will publish the acceptable key-value pair, for example
-- key = \"LakeFormationTrustedCaller\" and value = \"TRUE\" and the third
-- party integrator must properly tag the temporary security credentials
-- that will be used to call Lake Formation\'s administrative APIs.
dataLakeSettings_authorizedSessionTagValueList :: Lens.Lens' DataLakeSettings (Prelude.Maybe [Prelude.Text])
dataLakeSettings_authorizedSessionTagValueList = Lens.lens (\DataLakeSettings' {authorizedSessionTagValueList} -> authorizedSessionTagValueList) (\s@DataLakeSettings' {} a -> s {authorizedSessionTagValueList = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether access control on newly created database is managed by
-- Lake Formation permissions or exclusively by IAM permissions.
--
-- A null value indicates access control by Lake Formation permissions. A
-- value that assigns ALL to IAM_ALLOWED_PRINCIPALS indicates access
-- control by IAM permissions. This is referred to as the setting \"Use
-- only IAM access control,\" and is for backward compatibility with the
-- Glue permission model implemented by IAM permissions.
--
-- The only permitted values are an empty array or an array that contains a
-- single JSON object that grants ALL to IAM_ALLOWED_PRINCIPALS.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lake-formation/latest/dg/change-settings.html Changing the Default Security Settings for Your Data Lake>.
dataLakeSettings_createDatabaseDefaultPermissions :: Lens.Lens' DataLakeSettings (Prelude.Maybe [PrincipalPermissions])
dataLakeSettings_createDatabaseDefaultPermissions = Lens.lens (\DataLakeSettings' {createDatabaseDefaultPermissions} -> createDatabaseDefaultPermissions) (\s@DataLakeSettings' {} a -> s {createDatabaseDefaultPermissions = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether access control on newly created table is managed by
-- Lake Formation permissions or exclusively by IAM permissions.
--
-- A null value indicates access control by Lake Formation permissions. A
-- value that assigns ALL to IAM_ALLOWED_PRINCIPALS indicates access
-- control by IAM permissions. This is referred to as the setting \"Use
-- only IAM access control,\" and is for backward compatibility with the
-- Glue permission model implemented by IAM permissions.
--
-- The only permitted values are an empty array or an array that contains a
-- single JSON object that grants ALL to IAM_ALLOWED_PRINCIPALS.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lake-formation/latest/dg/change-settings.html Changing the Default Security Settings for Your Data Lake>.
dataLakeSettings_createTableDefaultPermissions :: Lens.Lens' DataLakeSettings (Prelude.Maybe [PrincipalPermissions])
dataLakeSettings_createTableDefaultPermissions = Lens.lens (\DataLakeSettings' {createTableDefaultPermissions} -> createTableDefaultPermissions) (\s@DataLakeSettings' {} a -> s {createTableDefaultPermissions = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

-- | A list of Lake Formation principals. Supported principals are IAM users
-- or IAM roles.
dataLakeSettings_dataLakeAdmins :: Lens.Lens' DataLakeSettings (Prelude.Maybe [DataLakePrincipal])
dataLakeSettings_dataLakeAdmins = Lens.lens (\DataLakeSettings' {dataLakeAdmins} -> dataLakeAdmins) (\s@DataLakeSettings' {} a -> s {dataLakeAdmins = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

-- | A list of the account IDs of Amazon Web Services accounts with Amazon
-- EMR clusters that are to perform data filtering.>
dataLakeSettings_externalDataFilteringAllowList :: Lens.Lens' DataLakeSettings (Prelude.Maybe [DataLakePrincipal])
dataLakeSettings_externalDataFilteringAllowList = Lens.lens (\DataLakeSettings' {externalDataFilteringAllowList} -> externalDataFilteringAllowList) (\s@DataLakeSettings' {} a -> s {externalDataFilteringAllowList = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

-- | A key-value map that provides an additional configuration on your data
-- lake. CrossAccountVersion is the key you can configure in the Parameters
-- field. Accepted values for the CrossAccountVersion key are 1, 2, and 3.
dataLakeSettings_parameters :: Lens.Lens' DataLakeSettings (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dataLakeSettings_parameters = Lens.lens (\DataLakeSettings' {parameters} -> parameters) (\s@DataLakeSettings' {} a -> s {parameters = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

-- | A list of the resource-owning account IDs that the caller\'s account can
-- use to share their user access details (user ARNs). The user ARNs can be
-- logged in the resource owner\'s CloudTrail log.
--
-- You may want to specify this property when you are in a high-trust
-- boundary, such as the same team or company.
dataLakeSettings_trustedResourceOwners :: Lens.Lens' DataLakeSettings (Prelude.Maybe [Prelude.Text])
dataLakeSettings_trustedResourceOwners = Lens.lens (\DataLakeSettings' {trustedResourceOwners} -> trustedResourceOwners) (\s@DataLakeSettings' {} a -> s {trustedResourceOwners = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DataLakeSettings where
  parseJSON =
    Data.withObject
      "DataLakeSettings"
      ( \x ->
          DataLakeSettings'
            Prelude.<$> (x Data..:? "AllowExternalDataFiltering")
            Prelude.<*> ( x
                            Data..:? "AuthorizedSessionTagValueList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "CreateDatabaseDefaultPermissions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "CreateTableDefaultPermissions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DataLakeAdmins" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "ExternalDataFilteringAllowList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "TrustedResourceOwners"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DataLakeSettings where
  hashWithSalt _salt DataLakeSettings' {..} =
    _salt
      `Prelude.hashWithSalt` allowExternalDataFiltering
      `Prelude.hashWithSalt` authorizedSessionTagValueList
      `Prelude.hashWithSalt` createDatabaseDefaultPermissions
      `Prelude.hashWithSalt` createTableDefaultPermissions
      `Prelude.hashWithSalt` dataLakeAdmins
      `Prelude.hashWithSalt` externalDataFilteringAllowList
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` trustedResourceOwners

instance Prelude.NFData DataLakeSettings where
  rnf DataLakeSettings' {..} =
    Prelude.rnf allowExternalDataFiltering
      `Prelude.seq` Prelude.rnf authorizedSessionTagValueList
      `Prelude.seq` Prelude.rnf createDatabaseDefaultPermissions
      `Prelude.seq` Prelude.rnf createTableDefaultPermissions
      `Prelude.seq` Prelude.rnf dataLakeAdmins
      `Prelude.seq` Prelude.rnf externalDataFilteringAllowList
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf trustedResourceOwners

instance Data.ToJSON DataLakeSettings where
  toJSON DataLakeSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowExternalDataFiltering" Data..=)
              Prelude.<$> allowExternalDataFiltering,
            ("AuthorizedSessionTagValueList" Data..=)
              Prelude.<$> authorizedSessionTagValueList,
            ("CreateDatabaseDefaultPermissions" Data..=)
              Prelude.<$> createDatabaseDefaultPermissions,
            ("CreateTableDefaultPermissions" Data..=)
              Prelude.<$> createTableDefaultPermissions,
            ("DataLakeAdmins" Data..=)
              Prelude.<$> dataLakeAdmins,
            ("ExternalDataFilteringAllowList" Data..=)
              Prelude.<$> externalDataFilteringAllowList,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("TrustedResourceOwners" Data..=)
              Prelude.<$> trustedResourceOwners
          ]
      )
