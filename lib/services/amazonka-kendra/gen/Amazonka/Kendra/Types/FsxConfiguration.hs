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
-- Module      : Amazonka.Kendra.Types.FsxConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FsxConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.DataSourceVpcConfiguration
import Amazonka.Kendra.Types.FsxFileSystemType
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to Amazon FSx as your
-- data source.
--
-- /See:/ 'newFsxConfiguration' smart constructor.
data FsxConfiguration = FsxConfiguration'
  { -- | A list of regular expression patterns to exclude certain files in your
    -- Amazon FSx file system. Files that match the patterns are excluded from
    -- the index. Files that don\'t match the patterns are included in the
    -- index. If a file matches both an inclusion and exclusion pattern, the
    -- exclusion pattern takes precedence and the file isn\'t included in the
    -- index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map Amazon FSx
    -- data source attributes or field names to Amazon Kendra index field
    -- names. To create custom fields, use the @UpdateIndex@ API before you map
    -- to Amazon FSx fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Amazon FSx data source field names must exist in your Amazon FSx
    -- custom metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to include certain files in your
    -- Amazon FSx file system. Files that match the patterns are included in
    -- the index. Files that don\'t match the patterns are excluded from the
    -- index. If a file matches both an inclusion and exclusion pattern, the
    -- exclusion pattern takes precedence and the file isn\'t included in the
    -- index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
    -- contains the key-value pairs required to connect to your Amazon FSx file
    -- system. Windows is currently the only supported type. The secret must
    -- contain a JSON structure with the following keys:
    --
    -- -   username—The Active Directory user name, along with the Domain Name
    --     System (DNS) domain name. For example, /user\@corp.example.com/. The
    --     Active Directory user account must have read and mounting access to
    --     the Amazon FSx file system for Windows.
    --
    -- -   password—The password of the Active Directory user account with read
    --     and mounting access to the Amazon FSx Windows file system.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon FSx file system.
    --
    -- You can find your file system ID on the file system dashboard in the
    -- Amazon FSx console. For information on how to create a file system in
    -- Amazon FSx console, using Windows File Server as an example, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/getting-started-step1.html Amazon FSx Getting started guide>.
    fileSystemId :: Prelude.Text,
    -- | The Amazon FSx file system type. Windows is currently the only supported
    -- type.
    fileSystemType :: FsxFileSystemType,
    -- | Configuration information for an Amazon Virtual Private Cloud to connect
    -- to your Amazon FSx. Your Amazon FSx instance must reside inside your
    -- VPC.
    vpcConfiguration :: DataSourceVpcConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FsxConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusionPatterns', 'fsxConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain files in your
-- Amazon FSx file system. Files that match the patterns are excluded from
-- the index. Files that don\'t match the patterns are included in the
-- index. If a file matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the file isn\'t included in the
-- index.
--
-- 'fieldMappings', 'fsxConfiguration_fieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map Amazon FSx
-- data source attributes or field names to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to Amazon FSx fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Amazon FSx data source field names must exist in your Amazon FSx
-- custom metadata.
--
-- 'inclusionPatterns', 'fsxConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain files in your
-- Amazon FSx file system. Files that match the patterns are included in
-- the index. Files that don\'t match the patterns are excluded from the
-- index. If a file matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the file isn\'t included in the
-- index.
--
-- 'secretArn', 'fsxConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your Amazon FSx file
-- system. Windows is currently the only supported type. The secret must
-- contain a JSON structure with the following keys:
--
-- -   username—The Active Directory user name, along with the Domain Name
--     System (DNS) domain name. For example, /user\@corp.example.com/. The
--     Active Directory user account must have read and mounting access to
--     the Amazon FSx file system for Windows.
--
-- -   password—The password of the Active Directory user account with read
--     and mounting access to the Amazon FSx Windows file system.
--
-- 'fileSystemId', 'fsxConfiguration_fileSystemId' - The identifier of the Amazon FSx file system.
--
-- You can find your file system ID on the file system dashboard in the
-- Amazon FSx console. For information on how to create a file system in
-- Amazon FSx console, using Windows File Server as an example, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/getting-started-step1.html Amazon FSx Getting started guide>.
--
-- 'fileSystemType', 'fsxConfiguration_fileSystemType' - The Amazon FSx file system type. Windows is currently the only supported
-- type.
--
-- 'vpcConfiguration', 'fsxConfiguration_vpcConfiguration' - Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Amazon FSx. Your Amazon FSx instance must reside inside your
-- VPC.
newFsxConfiguration ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'fileSystemType'
  FsxFileSystemType ->
  -- | 'vpcConfiguration'
  DataSourceVpcConfiguration ->
  FsxConfiguration
newFsxConfiguration
  pFileSystemId_
  pFileSystemType_
  pVpcConfiguration_ =
    FsxConfiguration'
      { exclusionPatterns =
          Prelude.Nothing,
        fieldMappings = Prelude.Nothing,
        inclusionPatterns = Prelude.Nothing,
        secretArn = Prelude.Nothing,
        fileSystemId = pFileSystemId_,
        fileSystemType = pFileSystemType_,
        vpcConfiguration = pVpcConfiguration_
      }

-- | A list of regular expression patterns to exclude certain files in your
-- Amazon FSx file system. Files that match the patterns are excluded from
-- the index. Files that don\'t match the patterns are included in the
-- index. If a file matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the file isn\'t included in the
-- index.
fsxConfiguration_exclusionPatterns :: Lens.Lens' FsxConfiguration (Prelude.Maybe [Prelude.Text])
fsxConfiguration_exclusionPatterns = Lens.lens (\FsxConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@FsxConfiguration' {} a -> s {exclusionPatterns = a} :: FsxConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map Amazon FSx
-- data source attributes or field names to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to Amazon FSx fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Amazon FSx data source field names must exist in your Amazon FSx
-- custom metadata.
fsxConfiguration_fieldMappings :: Lens.Lens' FsxConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
fsxConfiguration_fieldMappings = Lens.lens (\FsxConfiguration' {fieldMappings} -> fieldMappings) (\s@FsxConfiguration' {} a -> s {fieldMappings = a} :: FsxConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain files in your
-- Amazon FSx file system. Files that match the patterns are included in
-- the index. Files that don\'t match the patterns are excluded from the
-- index. If a file matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the file isn\'t included in the
-- index.
fsxConfiguration_inclusionPatterns :: Lens.Lens' FsxConfiguration (Prelude.Maybe [Prelude.Text])
fsxConfiguration_inclusionPatterns = Lens.lens (\FsxConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@FsxConfiguration' {} a -> s {inclusionPatterns = a} :: FsxConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your Amazon FSx file
-- system. Windows is currently the only supported type. The secret must
-- contain a JSON structure with the following keys:
--
-- -   username—The Active Directory user name, along with the Domain Name
--     System (DNS) domain name. For example, /user\@corp.example.com/. The
--     Active Directory user account must have read and mounting access to
--     the Amazon FSx file system for Windows.
--
-- -   password—The password of the Active Directory user account with read
--     and mounting access to the Amazon FSx Windows file system.
fsxConfiguration_secretArn :: Lens.Lens' FsxConfiguration (Prelude.Maybe Prelude.Text)
fsxConfiguration_secretArn = Lens.lens (\FsxConfiguration' {secretArn} -> secretArn) (\s@FsxConfiguration' {} a -> s {secretArn = a} :: FsxConfiguration)

-- | The identifier of the Amazon FSx file system.
--
-- You can find your file system ID on the file system dashboard in the
-- Amazon FSx console. For information on how to create a file system in
-- Amazon FSx console, using Windows File Server as an example, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/getting-started-step1.html Amazon FSx Getting started guide>.
fsxConfiguration_fileSystemId :: Lens.Lens' FsxConfiguration Prelude.Text
fsxConfiguration_fileSystemId = Lens.lens (\FsxConfiguration' {fileSystemId} -> fileSystemId) (\s@FsxConfiguration' {} a -> s {fileSystemId = a} :: FsxConfiguration)

-- | The Amazon FSx file system type. Windows is currently the only supported
-- type.
fsxConfiguration_fileSystemType :: Lens.Lens' FsxConfiguration FsxFileSystemType
fsxConfiguration_fileSystemType = Lens.lens (\FsxConfiguration' {fileSystemType} -> fileSystemType) (\s@FsxConfiguration' {} a -> s {fileSystemType = a} :: FsxConfiguration)

-- | Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Amazon FSx. Your Amazon FSx instance must reside inside your
-- VPC.
fsxConfiguration_vpcConfiguration :: Lens.Lens' FsxConfiguration DataSourceVpcConfiguration
fsxConfiguration_vpcConfiguration = Lens.lens (\FsxConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@FsxConfiguration' {} a -> s {vpcConfiguration = a} :: FsxConfiguration)

instance Data.FromJSON FsxConfiguration where
  parseJSON =
    Data.withObject
      "FsxConfiguration"
      ( \x ->
          FsxConfiguration'
            Prelude.<$> ( x
                            Data..:? "ExclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FieldMappings")
            Prelude.<*> ( x
                            Data..:? "InclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SecretArn")
            Prelude.<*> (x Data..: "FileSystemId")
            Prelude.<*> (x Data..: "FileSystemType")
            Prelude.<*> (x Data..: "VpcConfiguration")
      )

instance Prelude.Hashable FsxConfiguration where
  hashWithSalt _salt FsxConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` fieldMappings
      `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` fileSystemType
      `Prelude.hashWithSalt` vpcConfiguration

instance Prelude.NFData FsxConfiguration where
  rnf FsxConfiguration' {..} =
    Prelude.rnf exclusionPatterns `Prelude.seq`
      Prelude.rnf fieldMappings `Prelude.seq`
        Prelude.rnf inclusionPatterns `Prelude.seq`
          Prelude.rnf secretArn `Prelude.seq`
            Prelude.rnf fileSystemId `Prelude.seq`
              Prelude.rnf fileSystemType `Prelude.seq`
                Prelude.rnf vpcConfiguration

instance Data.ToJSON FsxConfiguration where
  toJSON FsxConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExclusionPatterns" Data..=)
              Prelude.<$> exclusionPatterns,
            ("FieldMappings" Data..=) Prelude.<$> fieldMappings,
            ("InclusionPatterns" Data..=)
              Prelude.<$> inclusionPatterns,
            ("SecretArn" Data..=) Prelude.<$> secretArn,
            Prelude.Just ("FileSystemId" Data..= fileSystemId),
            Prelude.Just
              ("FileSystemType" Data..= fileSystemType),
            Prelude.Just
              ("VpcConfiguration" Data..= vpcConfiguration)
          ]
      )
