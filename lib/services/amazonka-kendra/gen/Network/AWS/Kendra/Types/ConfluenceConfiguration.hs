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
-- Module      : Network.AWS.Kendra.Types.ConfluenceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ConfluenceConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ConfluenceAttachmentConfiguration
import Network.AWS.Kendra.Types.ConfluenceBlogConfiguration
import Network.AWS.Kendra.Types.ConfluencePageConfiguration
import Network.AWS.Kendra.Types.ConfluenceSpaceConfiguration
import Network.AWS.Kendra.Types.ConfluenceVersion
import Network.AWS.Kendra.Types.DataSourceVpcConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for data sources that connect to
-- Confluence.
--
-- /See:/ 'newConfluenceConfiguration' smart constructor.
data ConfluenceConfiguration = ConfluenceConfiguration'
  { -- | Specifies configuration information for indexing Confluence pages.
    pageConfiguration :: Prelude.Maybe ConfluencePageConfiguration,
    -- | Specifies configuration information for indexing attachments to
    -- Confluence blogs and pages.
    attachmentConfiguration :: Prelude.Maybe ConfluenceAttachmentConfiguration,
    -- | Specifies configuration information for indexing Confluence spaces.
    spaceConfiguration :: Prelude.Maybe ConfluenceSpaceConfiguration,
    -- | A list of regular expression patterns that apply to a URL on the
    -- Confluence server. An exclusion pattern can apply to a blog post, a
    -- page, a space, or an attachment. Items that match the pattern are
    -- excluded from the index. Items that don\'t match the pattern are
    -- included in the index. If a item matches both an exclusion pattern and
    -- an inclusion pattern, the item isn\'t included in the index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the information for connecting to an Amazon VPC.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | A list of regular expression patterns that apply to a URL on the
    -- Confluence server. An inclusion pattern can apply to a blog post, a
    -- page, a space, or an attachment. Items that match the patterns are
    -- included in the index. Items that don\'t match the pattern are excluded
    -- from the index. If an item matches both an inclusion pattern and an
    -- exclusion pattern, the item isn\'t included in the index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies configuration information for indexing Confluence blogs.
    blogConfiguration :: Prelude.Maybe ConfluenceBlogConfiguration,
    -- | The URL of your Confluence instance. Use the full URL of the server. For
    -- example, @https:\/\/server.example.com:port\/@. You can also use an IP
    -- address, for example, @https:\/\/192.168.1.113\/@.
    serverUrl :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Secrets Managersecret that contains
    -- the key\/value pairs required to connect to your Confluence server. The
    -- secret must contain a JSON structure with the following keys:
    --
    -- -   username - The user name or email address of a user with
    --     administrative privileges for the Confluence server.
    --
    -- -   password - The password associated with the user logging in to the
    --     Confluence server.
    secretArn :: Prelude.Text,
    -- | Specifies the version of the Confluence installation that you are
    -- connecting to.
    version :: ConfluenceVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfluenceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageConfiguration', 'confluenceConfiguration_pageConfiguration' - Specifies configuration information for indexing Confluence pages.
--
-- 'attachmentConfiguration', 'confluenceConfiguration_attachmentConfiguration' - Specifies configuration information for indexing attachments to
-- Confluence blogs and pages.
--
-- 'spaceConfiguration', 'confluenceConfiguration_spaceConfiguration' - Specifies configuration information for indexing Confluence spaces.
--
-- 'exclusionPatterns', 'confluenceConfiguration_exclusionPatterns' - A list of regular expression patterns that apply to a URL on the
-- Confluence server. An exclusion pattern can apply to a blog post, a
-- page, a space, or an attachment. Items that match the pattern are
-- excluded from the index. Items that don\'t match the pattern are
-- included in the index. If a item matches both an exclusion pattern and
-- an inclusion pattern, the item isn\'t included in the index.
--
-- 'vpcConfiguration', 'confluenceConfiguration_vpcConfiguration' - Specifies the information for connecting to an Amazon VPC.
--
-- 'inclusionPatterns', 'confluenceConfiguration_inclusionPatterns' - A list of regular expression patterns that apply to a URL on the
-- Confluence server. An inclusion pattern can apply to a blog post, a
-- page, a space, or an attachment. Items that match the patterns are
-- included in the index. Items that don\'t match the pattern are excluded
-- from the index. If an item matches both an inclusion pattern and an
-- exclusion pattern, the item isn\'t included in the index.
--
-- 'blogConfiguration', 'confluenceConfiguration_blogConfiguration' - Specifies configuration information for indexing Confluence blogs.
--
-- 'serverUrl', 'confluenceConfiguration_serverUrl' - The URL of your Confluence instance. Use the full URL of the server. For
-- example, @https:\/\/server.example.com:port\/@. You can also use an IP
-- address, for example, @https:\/\/192.168.1.113\/@.
--
-- 'secretArn', 'confluenceConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Managersecret that contains
-- the key\/value pairs required to connect to your Confluence server. The
-- secret must contain a JSON structure with the following keys:
--
-- -   username - The user name or email address of a user with
--     administrative privileges for the Confluence server.
--
-- -   password - The password associated with the user logging in to the
--     Confluence server.
--
-- 'version', 'confluenceConfiguration_version' - Specifies the version of the Confluence installation that you are
-- connecting to.
newConfluenceConfiguration ::
  -- | 'serverUrl'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  -- | 'version'
  ConfluenceVersion ->
  ConfluenceConfiguration
newConfluenceConfiguration
  pServerUrl_
  pSecretArn_
  pVersion_ =
    ConfluenceConfiguration'
      { pageConfiguration =
          Prelude.Nothing,
        attachmentConfiguration = Prelude.Nothing,
        spaceConfiguration = Prelude.Nothing,
        exclusionPatterns = Prelude.Nothing,
        vpcConfiguration = Prelude.Nothing,
        inclusionPatterns = Prelude.Nothing,
        blogConfiguration = Prelude.Nothing,
        serverUrl = pServerUrl_,
        secretArn = pSecretArn_,
        version = pVersion_
      }

-- | Specifies configuration information for indexing Confluence pages.
confluenceConfiguration_pageConfiguration :: Lens.Lens' ConfluenceConfiguration (Prelude.Maybe ConfluencePageConfiguration)
confluenceConfiguration_pageConfiguration = Lens.lens (\ConfluenceConfiguration' {pageConfiguration} -> pageConfiguration) (\s@ConfluenceConfiguration' {} a -> s {pageConfiguration = a} :: ConfluenceConfiguration)

-- | Specifies configuration information for indexing attachments to
-- Confluence blogs and pages.
confluenceConfiguration_attachmentConfiguration :: Lens.Lens' ConfluenceConfiguration (Prelude.Maybe ConfluenceAttachmentConfiguration)
confluenceConfiguration_attachmentConfiguration = Lens.lens (\ConfluenceConfiguration' {attachmentConfiguration} -> attachmentConfiguration) (\s@ConfluenceConfiguration' {} a -> s {attachmentConfiguration = a} :: ConfluenceConfiguration)

-- | Specifies configuration information for indexing Confluence spaces.
confluenceConfiguration_spaceConfiguration :: Lens.Lens' ConfluenceConfiguration (Prelude.Maybe ConfluenceSpaceConfiguration)
confluenceConfiguration_spaceConfiguration = Lens.lens (\ConfluenceConfiguration' {spaceConfiguration} -> spaceConfiguration) (\s@ConfluenceConfiguration' {} a -> s {spaceConfiguration = a} :: ConfluenceConfiguration)

-- | A list of regular expression patterns that apply to a URL on the
-- Confluence server. An exclusion pattern can apply to a blog post, a
-- page, a space, or an attachment. Items that match the pattern are
-- excluded from the index. Items that don\'t match the pattern are
-- included in the index. If a item matches both an exclusion pattern and
-- an inclusion pattern, the item isn\'t included in the index.
confluenceConfiguration_exclusionPatterns :: Lens.Lens' ConfluenceConfiguration (Prelude.Maybe [Prelude.Text])
confluenceConfiguration_exclusionPatterns = Lens.lens (\ConfluenceConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@ConfluenceConfiguration' {} a -> s {exclusionPatterns = a} :: ConfluenceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the information for connecting to an Amazon VPC.
confluenceConfiguration_vpcConfiguration :: Lens.Lens' ConfluenceConfiguration (Prelude.Maybe DataSourceVpcConfiguration)
confluenceConfiguration_vpcConfiguration = Lens.lens (\ConfluenceConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@ConfluenceConfiguration' {} a -> s {vpcConfiguration = a} :: ConfluenceConfiguration)

-- | A list of regular expression patterns that apply to a URL on the
-- Confluence server. An inclusion pattern can apply to a blog post, a
-- page, a space, or an attachment. Items that match the patterns are
-- included in the index. Items that don\'t match the pattern are excluded
-- from the index. If an item matches both an inclusion pattern and an
-- exclusion pattern, the item isn\'t included in the index.
confluenceConfiguration_inclusionPatterns :: Lens.Lens' ConfluenceConfiguration (Prelude.Maybe [Prelude.Text])
confluenceConfiguration_inclusionPatterns = Lens.lens (\ConfluenceConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@ConfluenceConfiguration' {} a -> s {inclusionPatterns = a} :: ConfluenceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies configuration information for indexing Confluence blogs.
confluenceConfiguration_blogConfiguration :: Lens.Lens' ConfluenceConfiguration (Prelude.Maybe ConfluenceBlogConfiguration)
confluenceConfiguration_blogConfiguration = Lens.lens (\ConfluenceConfiguration' {blogConfiguration} -> blogConfiguration) (\s@ConfluenceConfiguration' {} a -> s {blogConfiguration = a} :: ConfluenceConfiguration)

-- | The URL of your Confluence instance. Use the full URL of the server. For
-- example, @https:\/\/server.example.com:port\/@. You can also use an IP
-- address, for example, @https:\/\/192.168.1.113\/@.
confluenceConfiguration_serverUrl :: Lens.Lens' ConfluenceConfiguration Prelude.Text
confluenceConfiguration_serverUrl = Lens.lens (\ConfluenceConfiguration' {serverUrl} -> serverUrl) (\s@ConfluenceConfiguration' {} a -> s {serverUrl = a} :: ConfluenceConfiguration)

-- | The Amazon Resource Name (ARN) of an Secrets Managersecret that contains
-- the key\/value pairs required to connect to your Confluence server. The
-- secret must contain a JSON structure with the following keys:
--
-- -   username - The user name or email address of a user with
--     administrative privileges for the Confluence server.
--
-- -   password - The password associated with the user logging in to the
--     Confluence server.
confluenceConfiguration_secretArn :: Lens.Lens' ConfluenceConfiguration Prelude.Text
confluenceConfiguration_secretArn = Lens.lens (\ConfluenceConfiguration' {secretArn} -> secretArn) (\s@ConfluenceConfiguration' {} a -> s {secretArn = a} :: ConfluenceConfiguration)

-- | Specifies the version of the Confluence installation that you are
-- connecting to.
confluenceConfiguration_version :: Lens.Lens' ConfluenceConfiguration ConfluenceVersion
confluenceConfiguration_version = Lens.lens (\ConfluenceConfiguration' {version} -> version) (\s@ConfluenceConfiguration' {} a -> s {version = a} :: ConfluenceConfiguration)

instance Core.FromJSON ConfluenceConfiguration where
  parseJSON =
    Core.withObject
      "ConfluenceConfiguration"
      ( \x ->
          ConfluenceConfiguration'
            Prelude.<$> (x Core..:? "PageConfiguration")
            Prelude.<*> (x Core..:? "AttachmentConfiguration")
            Prelude.<*> (x Core..:? "SpaceConfiguration")
            Prelude.<*> ( x Core..:? "ExclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "VpcConfiguration")
            Prelude.<*> ( x Core..:? "InclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "BlogConfiguration")
            Prelude.<*> (x Core..: "ServerUrl")
            Prelude.<*> (x Core..: "SecretArn")
            Prelude.<*> (x Core..: "Version")
      )

instance Prelude.Hashable ConfluenceConfiguration

instance Prelude.NFData ConfluenceConfiguration

instance Core.ToJSON ConfluenceConfiguration where
  toJSON ConfluenceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageConfiguration" Core..=)
              Prelude.<$> pageConfiguration,
            ("AttachmentConfiguration" Core..=)
              Prelude.<$> attachmentConfiguration,
            ("SpaceConfiguration" Core..=)
              Prelude.<$> spaceConfiguration,
            ("ExclusionPatterns" Core..=)
              Prelude.<$> exclusionPatterns,
            ("VpcConfiguration" Core..=)
              Prelude.<$> vpcConfiguration,
            ("InclusionPatterns" Core..=)
              Prelude.<$> inclusionPatterns,
            ("BlogConfiguration" Core..=)
              Prelude.<$> blogConfiguration,
            Prelude.Just ("ServerUrl" Core..= serverUrl),
            Prelude.Just ("SecretArn" Core..= secretArn),
            Prelude.Just ("Version" Core..= version)
          ]
      )
