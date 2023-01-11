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
-- Module      : Amazonka.Kendra.Types.BoxConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.BoxConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.DataSourceVpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to Box as your data
-- source.
--
-- /See:/ 'newBoxConfiguration' smart constructor.
data BoxConfiguration = BoxConfiguration'
  { -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of Box comments to Amazon Kendra index field names. To
    -- create custom fields, use the @UpdateIndex@ API before you map to Box
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Box field names must exist in your Box custom metadata.
    commentFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | @TRUE@ to index comments.
    crawlComments :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to index the contents of tasks.
    crawlTasks :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to index web links.
    crawlWebLinks :: Prelude.Maybe Prelude.Bool,
    -- | A list of regular expression patterns to exclude certain files and
    -- folders from your Box platform. Files and folders that match the
    -- patterns are excluded from the index.Files and folders that don\'t match
    -- the patterns are included in the index. If a file or folder matches both
    -- an inclusion and exclusion pattern, the exclusion pattern takes
    -- precedence and the file or folder isn\'t included in the index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of Box files to Amazon Kendra index field names. To create
    -- custom fields, use the @UpdateIndex@ API before you map to Box fields.
    -- For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Box field names must exist in your Box custom metadata.
    fileFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to include certain files and
    -- folders in your Box platform. Files and folders that match the patterns
    -- are included in the index. Files and folders that don\'t match the
    -- patterns are excluded from the index. If a file or folder matches both
    -- an inclusion and exclusion pattern, the exclusion pattern takes
    -- precedence and the file or folder isn\'t included in the index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of Box tasks to Amazon Kendra index field names. To create
    -- custom fields, use the @UpdateIndex@ API before you map to Box fields.
    -- For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Box field names must exist in your Box custom metadata.
    taskFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | @TRUE@ to use the Slack change log to determine which documents require
    -- updating in the index. Depending on the data source change log\'s size,
    -- it may take longer for Amazon Kendra to use the change log than to scan
    -- all of your documents.
    useChangeLog :: Prelude.Maybe Prelude.Bool,
    -- | Configuration information for an Amazon VPC to connect to your Box. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of Box web links to Amazon Kendra index field names. To
    -- create custom fields, use the @UpdateIndex@ API before you map to Box
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Box field names must exist in your Box custom metadata.
    webLinkFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | The identifier of the Box Enterprise platform. You can find the
    -- enterprise ID in the Box Developer Console settings or when you create
    -- an app in Box and download your authentication credentials. For example,
    -- /801234567/.
    enterpriseId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
    -- contains the key-value pairs required to connect to your Box platform.
    -- The secret must contain a JSON structure with the following keys:
    --
    -- -   clientID—The identifier of the client OAuth 2.0 authentication
    --     application created in Box.
    --
    -- -   clientSecret—A set of characters known only to the OAuth 2.0
    --     authentication application created in Box.
    --
    -- -   publicKeyId—The identifier of the public key contained within an
    --     identity certificate.
    --
    -- -   privateKey—A set of characters that make up an encryption key.
    --
    -- -   passphrase—A set of characters that act like a password.
    --
    -- You create an application in Box to generate the keys or credentials
    -- required for the secret. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-box.html Using a Box data source>.
    secretArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BoxConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentFieldMappings', 'boxConfiguration_commentFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Box comments to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to Box
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Box field names must exist in your Box custom metadata.
--
-- 'crawlComments', 'boxConfiguration_crawlComments' - @TRUE@ to index comments.
--
-- 'crawlTasks', 'boxConfiguration_crawlTasks' - @TRUE@ to index the contents of tasks.
--
-- 'crawlWebLinks', 'boxConfiguration_crawlWebLinks' - @TRUE@ to index web links.
--
-- 'exclusionPatterns', 'boxConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain files and
-- folders from your Box platform. Files and folders that match the
-- patterns are excluded from the index.Files and folders that don\'t match
-- the patterns are included in the index. If a file or folder matches both
-- an inclusion and exclusion pattern, the exclusion pattern takes
-- precedence and the file or folder isn\'t included in the index.
--
-- 'fileFieldMappings', 'boxConfiguration_fileFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Box files to Amazon Kendra index field names. To create
-- custom fields, use the @UpdateIndex@ API before you map to Box fields.
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Box field names must exist in your Box custom metadata.
--
-- 'inclusionPatterns', 'boxConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain files and
-- folders in your Box platform. Files and folders that match the patterns
-- are included in the index. Files and folders that don\'t match the
-- patterns are excluded from the index. If a file or folder matches both
-- an inclusion and exclusion pattern, the exclusion pattern takes
-- precedence and the file or folder isn\'t included in the index.
--
-- 'taskFieldMappings', 'boxConfiguration_taskFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Box tasks to Amazon Kendra index field names. To create
-- custom fields, use the @UpdateIndex@ API before you map to Box fields.
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Box field names must exist in your Box custom metadata.
--
-- 'useChangeLog', 'boxConfiguration_useChangeLog' - @TRUE@ to use the Slack change log to determine which documents require
-- updating in the index. Depending on the data source change log\'s size,
-- it may take longer for Amazon Kendra to use the change log than to scan
-- all of your documents.
--
-- 'vpcConfiguration', 'boxConfiguration_vpcConfiguration' - Configuration information for an Amazon VPC to connect to your Box. For
-- more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
--
-- 'webLinkFieldMappings', 'boxConfiguration_webLinkFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Box web links to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to Box
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Box field names must exist in your Box custom metadata.
--
-- 'enterpriseId', 'boxConfiguration_enterpriseId' - The identifier of the Box Enterprise platform. You can find the
-- enterprise ID in the Box Developer Console settings or when you create
-- an app in Box and download your authentication credentials. For example,
-- /801234567/.
--
-- 'secretArn', 'boxConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your Box platform.
-- The secret must contain a JSON structure with the following keys:
--
-- -   clientID—The identifier of the client OAuth 2.0 authentication
--     application created in Box.
--
-- -   clientSecret—A set of characters known only to the OAuth 2.0
--     authentication application created in Box.
--
-- -   publicKeyId—The identifier of the public key contained within an
--     identity certificate.
--
-- -   privateKey—A set of characters that make up an encryption key.
--
-- -   passphrase—A set of characters that act like a password.
--
-- You create an application in Box to generate the keys or credentials
-- required for the secret. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-box.html Using a Box data source>.
newBoxConfiguration ::
  -- | 'enterpriseId'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  BoxConfiguration
newBoxConfiguration pEnterpriseId_ pSecretArn_ =
  BoxConfiguration'
    { commentFieldMappings =
        Prelude.Nothing,
      crawlComments = Prelude.Nothing,
      crawlTasks = Prelude.Nothing,
      crawlWebLinks = Prelude.Nothing,
      exclusionPatterns = Prelude.Nothing,
      fileFieldMappings = Prelude.Nothing,
      inclusionPatterns = Prelude.Nothing,
      taskFieldMappings = Prelude.Nothing,
      useChangeLog = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing,
      webLinkFieldMappings = Prelude.Nothing,
      enterpriseId = pEnterpriseId_,
      secretArn = pSecretArn_
    }

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Box comments to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to Box
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Box field names must exist in your Box custom metadata.
boxConfiguration_commentFieldMappings :: Lens.Lens' BoxConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
boxConfiguration_commentFieldMappings = Lens.lens (\BoxConfiguration' {commentFieldMappings} -> commentFieldMappings) (\s@BoxConfiguration' {} a -> s {commentFieldMappings = a} :: BoxConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to index comments.
boxConfiguration_crawlComments :: Lens.Lens' BoxConfiguration (Prelude.Maybe Prelude.Bool)
boxConfiguration_crawlComments = Lens.lens (\BoxConfiguration' {crawlComments} -> crawlComments) (\s@BoxConfiguration' {} a -> s {crawlComments = a} :: BoxConfiguration)

-- | @TRUE@ to index the contents of tasks.
boxConfiguration_crawlTasks :: Lens.Lens' BoxConfiguration (Prelude.Maybe Prelude.Bool)
boxConfiguration_crawlTasks = Lens.lens (\BoxConfiguration' {crawlTasks} -> crawlTasks) (\s@BoxConfiguration' {} a -> s {crawlTasks = a} :: BoxConfiguration)

-- | @TRUE@ to index web links.
boxConfiguration_crawlWebLinks :: Lens.Lens' BoxConfiguration (Prelude.Maybe Prelude.Bool)
boxConfiguration_crawlWebLinks = Lens.lens (\BoxConfiguration' {crawlWebLinks} -> crawlWebLinks) (\s@BoxConfiguration' {} a -> s {crawlWebLinks = a} :: BoxConfiguration)

-- | A list of regular expression patterns to exclude certain files and
-- folders from your Box platform. Files and folders that match the
-- patterns are excluded from the index.Files and folders that don\'t match
-- the patterns are included in the index. If a file or folder matches both
-- an inclusion and exclusion pattern, the exclusion pattern takes
-- precedence and the file or folder isn\'t included in the index.
boxConfiguration_exclusionPatterns :: Lens.Lens' BoxConfiguration (Prelude.Maybe [Prelude.Text])
boxConfiguration_exclusionPatterns = Lens.lens (\BoxConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@BoxConfiguration' {} a -> s {exclusionPatterns = a} :: BoxConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Box files to Amazon Kendra index field names. To create
-- custom fields, use the @UpdateIndex@ API before you map to Box fields.
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Box field names must exist in your Box custom metadata.
boxConfiguration_fileFieldMappings :: Lens.Lens' BoxConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
boxConfiguration_fileFieldMappings = Lens.lens (\BoxConfiguration' {fileFieldMappings} -> fileFieldMappings) (\s@BoxConfiguration' {} a -> s {fileFieldMappings = a} :: BoxConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain files and
-- folders in your Box platform. Files and folders that match the patterns
-- are included in the index. Files and folders that don\'t match the
-- patterns are excluded from the index. If a file or folder matches both
-- an inclusion and exclusion pattern, the exclusion pattern takes
-- precedence and the file or folder isn\'t included in the index.
boxConfiguration_inclusionPatterns :: Lens.Lens' BoxConfiguration (Prelude.Maybe [Prelude.Text])
boxConfiguration_inclusionPatterns = Lens.lens (\BoxConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@BoxConfiguration' {} a -> s {inclusionPatterns = a} :: BoxConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Box tasks to Amazon Kendra index field names. To create
-- custom fields, use the @UpdateIndex@ API before you map to Box fields.
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Box field names must exist in your Box custom metadata.
boxConfiguration_taskFieldMappings :: Lens.Lens' BoxConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
boxConfiguration_taskFieldMappings = Lens.lens (\BoxConfiguration' {taskFieldMappings} -> taskFieldMappings) (\s@BoxConfiguration' {} a -> s {taskFieldMappings = a} :: BoxConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to use the Slack change log to determine which documents require
-- updating in the index. Depending on the data source change log\'s size,
-- it may take longer for Amazon Kendra to use the change log than to scan
-- all of your documents.
boxConfiguration_useChangeLog :: Lens.Lens' BoxConfiguration (Prelude.Maybe Prelude.Bool)
boxConfiguration_useChangeLog = Lens.lens (\BoxConfiguration' {useChangeLog} -> useChangeLog) (\s@BoxConfiguration' {} a -> s {useChangeLog = a} :: BoxConfiguration)

-- | Configuration information for an Amazon VPC to connect to your Box. For
-- more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
boxConfiguration_vpcConfiguration :: Lens.Lens' BoxConfiguration (Prelude.Maybe DataSourceVpcConfiguration)
boxConfiguration_vpcConfiguration = Lens.lens (\BoxConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@BoxConfiguration' {} a -> s {vpcConfiguration = a} :: BoxConfiguration)

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of Box web links to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to Box
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Box field names must exist in your Box custom metadata.
boxConfiguration_webLinkFieldMappings :: Lens.Lens' BoxConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
boxConfiguration_webLinkFieldMappings = Lens.lens (\BoxConfiguration' {webLinkFieldMappings} -> webLinkFieldMappings) (\s@BoxConfiguration' {} a -> s {webLinkFieldMappings = a} :: BoxConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Box Enterprise platform. You can find the
-- enterprise ID in the Box Developer Console settings or when you create
-- an app in Box and download your authentication credentials. For example,
-- /801234567/.
boxConfiguration_enterpriseId :: Lens.Lens' BoxConfiguration Prelude.Text
boxConfiguration_enterpriseId = Lens.lens (\BoxConfiguration' {enterpriseId} -> enterpriseId) (\s@BoxConfiguration' {} a -> s {enterpriseId = a} :: BoxConfiguration)

-- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your Box platform.
-- The secret must contain a JSON structure with the following keys:
--
-- -   clientID—The identifier of the client OAuth 2.0 authentication
--     application created in Box.
--
-- -   clientSecret—A set of characters known only to the OAuth 2.0
--     authentication application created in Box.
--
-- -   publicKeyId—The identifier of the public key contained within an
--     identity certificate.
--
-- -   privateKey—A set of characters that make up an encryption key.
--
-- -   passphrase—A set of characters that act like a password.
--
-- You create an application in Box to generate the keys or credentials
-- required for the secret. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-box.html Using a Box data source>.
boxConfiguration_secretArn :: Lens.Lens' BoxConfiguration Prelude.Text
boxConfiguration_secretArn = Lens.lens (\BoxConfiguration' {secretArn} -> secretArn) (\s@BoxConfiguration' {} a -> s {secretArn = a} :: BoxConfiguration)

instance Data.FromJSON BoxConfiguration where
  parseJSON =
    Data.withObject
      "BoxConfiguration"
      ( \x ->
          BoxConfiguration'
            Prelude.<$> (x Data..:? "CommentFieldMappings")
            Prelude.<*> (x Data..:? "CrawlComments")
            Prelude.<*> (x Data..:? "CrawlTasks")
            Prelude.<*> (x Data..:? "CrawlWebLinks")
            Prelude.<*> ( x Data..:? "ExclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FileFieldMappings")
            Prelude.<*> ( x Data..:? "InclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TaskFieldMappings")
            Prelude.<*> (x Data..:? "UseChangeLog")
            Prelude.<*> (x Data..:? "VpcConfiguration")
            Prelude.<*> (x Data..:? "WebLinkFieldMappings")
            Prelude.<*> (x Data..: "EnterpriseId")
            Prelude.<*> (x Data..: "SecretArn")
      )

instance Prelude.Hashable BoxConfiguration where
  hashWithSalt _salt BoxConfiguration' {..} =
    _salt `Prelude.hashWithSalt` commentFieldMappings
      `Prelude.hashWithSalt` crawlComments
      `Prelude.hashWithSalt` crawlTasks
      `Prelude.hashWithSalt` crawlWebLinks
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` fileFieldMappings
      `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` taskFieldMappings
      `Prelude.hashWithSalt` useChangeLog
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` webLinkFieldMappings
      `Prelude.hashWithSalt` enterpriseId
      `Prelude.hashWithSalt` secretArn

instance Prelude.NFData BoxConfiguration where
  rnf BoxConfiguration' {..} =
    Prelude.rnf commentFieldMappings
      `Prelude.seq` Prelude.rnf crawlComments
      `Prelude.seq` Prelude.rnf crawlTasks
      `Prelude.seq` Prelude.rnf crawlWebLinks
      `Prelude.seq` Prelude.rnf exclusionPatterns
      `Prelude.seq` Prelude.rnf fileFieldMappings
      `Prelude.seq` Prelude.rnf inclusionPatterns
      `Prelude.seq` Prelude.rnf taskFieldMappings
      `Prelude.seq` Prelude.rnf useChangeLog
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf webLinkFieldMappings
      `Prelude.seq` Prelude.rnf enterpriseId
      `Prelude.seq` Prelude.rnf secretArn

instance Data.ToJSON BoxConfiguration where
  toJSON BoxConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CommentFieldMappings" Data..=)
              Prelude.<$> commentFieldMappings,
            ("CrawlComments" Data..=) Prelude.<$> crawlComments,
            ("CrawlTasks" Data..=) Prelude.<$> crawlTasks,
            ("CrawlWebLinks" Data..=) Prelude.<$> crawlWebLinks,
            ("ExclusionPatterns" Data..=)
              Prelude.<$> exclusionPatterns,
            ("FileFieldMappings" Data..=)
              Prelude.<$> fileFieldMappings,
            ("InclusionPatterns" Data..=)
              Prelude.<$> inclusionPatterns,
            ("TaskFieldMappings" Data..=)
              Prelude.<$> taskFieldMappings,
            ("UseChangeLog" Data..=) Prelude.<$> useChangeLog,
            ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            ("WebLinkFieldMappings" Data..=)
              Prelude.<$> webLinkFieldMappings,
            Prelude.Just ("EnterpriseId" Data..= enterpriseId),
            Prelude.Just ("SecretArn" Data..= secretArn)
          ]
      )
