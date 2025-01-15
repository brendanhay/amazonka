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
-- Module      : Amazonka.Kendra.Types.SlackConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SlackConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.DataSourceVpcConfiguration
import Amazonka.Kendra.Types.SlackEntity
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to Slack as your data
-- source.
--
-- /See:/ 'newSlackConfiguration' smart constructor.
data SlackConfiguration = SlackConfiguration'
  { -- | @TRUE@ to index bot messages from your Slack workspace team.
    crawlBotMessage :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to exclude archived messages to index from your Slack workspace
    -- team.
    excludeArchived :: Prelude.Maybe Prelude.Bool,
    -- | A list of regular expression patterns to exclude certain attached files
    -- in your Slack workspace team. Files that match the patterns are excluded
    -- from the index. Files that don’t match the patterns are included in the
    -- index. If a file matches both an inclusion and exclusion pattern, the
    -- exclusion pattern takes precedence and the file isn\'t included in the
    -- index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map Slack data
    -- source attributes or field names to Amazon Kendra index field names. To
    -- create custom fields, use the @UpdateIndex@ API before you map to Slack
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Slack data source field names must exist in your Slack custom
    -- metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to include certain attached files
    -- in your Slack workspace team. Files that match the patterns are included
    -- in the index. Files that don\'t match the patterns are excluded from the
    -- index. If a file matches both an inclusion and exclusion pattern, the
    -- exclusion pattern takes precedence and the file isn\'t included in the
    -- index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The number of hours for change log to look back from when you last
    -- synchronized your data. You can look back up to 7 days or 168 hours.
    --
    -- Change log updates your index only if new content was added since you
    -- last synced your data. Updated or deleted content from before you last
    -- synced does not get updated in your index. To capture updated or deleted
    -- content before you last synced, set the @LookBackPeriod@ to the number
    -- of hours you want change log to look back.
    lookBackPeriod :: Prelude.Maybe Prelude.Natural,
    -- | The list of private channel names from your Slack workspace team. You
    -- use this if you want to index specific private channels, not all private
    -- channels. You can also use regular expression patterns to filter private
    -- channels.
    privateChannelFilter :: Prelude.Maybe [Prelude.Text],
    -- | The list of public channel names to index from your Slack workspace
    -- team. You use this if you want to index specific public channels, not
    -- all public channels. You can also use regular expression patterns to
    -- filter public channels.
    publicChannelFilter :: Prelude.Maybe [Prelude.Text],
    -- | @TRUE@ to use the Slack change log to determine which documents require
    -- updating in the index. Depending on the Slack change log\'s size, it may
    -- take longer for Amazon Kendra to use the change log than to scan all of
    -- your documents in Slack.
    useChangeLog :: Prelude.Maybe Prelude.Bool,
    -- | Configuration information for an Amazon Virtual Private Cloud to connect
    -- to your Slack. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | The identifier of the team in the Slack workspace. For example,
    -- /T0123456789/.
    --
    -- You can find your team ID in the URL of the main page of your Slack
    -- workspace. When you log in to Slack via a browser, you are directed to
    -- the URL of the main page. For example,
    -- /https:\/\/app.slack.com\/client\/__T0123456789__\/.../.
    teamId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
    -- contains the key-value pairs required to connect to your Slack workspace
    -- team. The secret must contain a JSON structure with the following keys:
    --
    -- -   slackToken—The user or bot token created in Slack. For more
    --     information on creating a token in Slack, see
    --     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-slack.html#slack-authentication Authentication for a Slack data source>.
    secretArn :: Prelude.Text,
    -- | Specify whether to index public channels, private channels, group
    -- messages, and direct messages. You can specify one or more of these
    -- options.
    slackEntityList :: Prelude.NonEmpty SlackEntity,
    -- | The date to start crawling your data from your Slack workspace team. The
    -- date must follow this format: @yyyy-mm-dd@.
    sinceCrawlDate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlackConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlBotMessage', 'slackConfiguration_crawlBotMessage' - @TRUE@ to index bot messages from your Slack workspace team.
--
-- 'excludeArchived', 'slackConfiguration_excludeArchived' - @TRUE@ to exclude archived messages to index from your Slack workspace
-- team.
--
-- 'exclusionPatterns', 'slackConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain attached files
-- in your Slack workspace team. Files that match the patterns are excluded
-- from the index. Files that don’t match the patterns are included in the
-- index. If a file matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the file isn\'t included in the
-- index.
--
-- 'fieldMappings', 'slackConfiguration_fieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map Slack data
-- source attributes or field names to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to Slack
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Slack data source field names must exist in your Slack custom
-- metadata.
--
-- 'inclusionPatterns', 'slackConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain attached files
-- in your Slack workspace team. Files that match the patterns are included
-- in the index. Files that don\'t match the patterns are excluded from the
-- index. If a file matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the file isn\'t included in the
-- index.
--
-- 'lookBackPeriod', 'slackConfiguration_lookBackPeriod' - The number of hours for change log to look back from when you last
-- synchronized your data. You can look back up to 7 days or 168 hours.
--
-- Change log updates your index only if new content was added since you
-- last synced your data. Updated or deleted content from before you last
-- synced does not get updated in your index. To capture updated or deleted
-- content before you last synced, set the @LookBackPeriod@ to the number
-- of hours you want change log to look back.
--
-- 'privateChannelFilter', 'slackConfiguration_privateChannelFilter' - The list of private channel names from your Slack workspace team. You
-- use this if you want to index specific private channels, not all private
-- channels. You can also use regular expression patterns to filter private
-- channels.
--
-- 'publicChannelFilter', 'slackConfiguration_publicChannelFilter' - The list of public channel names to index from your Slack workspace
-- team. You use this if you want to index specific public channels, not
-- all public channels. You can also use regular expression patterns to
-- filter public channels.
--
-- 'useChangeLog', 'slackConfiguration_useChangeLog' - @TRUE@ to use the Slack change log to determine which documents require
-- updating in the index. Depending on the Slack change log\'s size, it may
-- take longer for Amazon Kendra to use the change log than to scan all of
-- your documents in Slack.
--
-- 'vpcConfiguration', 'slackConfiguration_vpcConfiguration' - Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Slack. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
--
-- 'teamId', 'slackConfiguration_teamId' - The identifier of the team in the Slack workspace. For example,
-- /T0123456789/.
--
-- You can find your team ID in the URL of the main page of your Slack
-- workspace. When you log in to Slack via a browser, you are directed to
-- the URL of the main page. For example,
-- /https:\/\/app.slack.com\/client\/__T0123456789__\/.../.
--
-- 'secretArn', 'slackConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your Slack workspace
-- team. The secret must contain a JSON structure with the following keys:
--
-- -   slackToken—The user or bot token created in Slack. For more
--     information on creating a token in Slack, see
--     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-slack.html#slack-authentication Authentication for a Slack data source>.
--
-- 'slackEntityList', 'slackConfiguration_slackEntityList' - Specify whether to index public channels, private channels, group
-- messages, and direct messages. You can specify one or more of these
-- options.
--
-- 'sinceCrawlDate', 'slackConfiguration_sinceCrawlDate' - The date to start crawling your data from your Slack workspace team. The
-- date must follow this format: @yyyy-mm-dd@.
newSlackConfiguration ::
  -- | 'teamId'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  -- | 'slackEntityList'
  Prelude.NonEmpty SlackEntity ->
  -- | 'sinceCrawlDate'
  Prelude.Text ->
  SlackConfiguration
newSlackConfiguration
  pTeamId_
  pSecretArn_
  pSlackEntityList_
  pSinceCrawlDate_ =
    SlackConfiguration'
      { crawlBotMessage =
          Prelude.Nothing,
        excludeArchived = Prelude.Nothing,
        exclusionPatterns = Prelude.Nothing,
        fieldMappings = Prelude.Nothing,
        inclusionPatterns = Prelude.Nothing,
        lookBackPeriod = Prelude.Nothing,
        privateChannelFilter = Prelude.Nothing,
        publicChannelFilter = Prelude.Nothing,
        useChangeLog = Prelude.Nothing,
        vpcConfiguration = Prelude.Nothing,
        teamId = pTeamId_,
        secretArn = pSecretArn_,
        slackEntityList =
          Lens.coerced Lens.# pSlackEntityList_,
        sinceCrawlDate = pSinceCrawlDate_
      }

-- | @TRUE@ to index bot messages from your Slack workspace team.
slackConfiguration_crawlBotMessage :: Lens.Lens' SlackConfiguration (Prelude.Maybe Prelude.Bool)
slackConfiguration_crawlBotMessage = Lens.lens (\SlackConfiguration' {crawlBotMessage} -> crawlBotMessage) (\s@SlackConfiguration' {} a -> s {crawlBotMessage = a} :: SlackConfiguration)

-- | @TRUE@ to exclude archived messages to index from your Slack workspace
-- team.
slackConfiguration_excludeArchived :: Lens.Lens' SlackConfiguration (Prelude.Maybe Prelude.Bool)
slackConfiguration_excludeArchived = Lens.lens (\SlackConfiguration' {excludeArchived} -> excludeArchived) (\s@SlackConfiguration' {} a -> s {excludeArchived = a} :: SlackConfiguration)

-- | A list of regular expression patterns to exclude certain attached files
-- in your Slack workspace team. Files that match the patterns are excluded
-- from the index. Files that don’t match the patterns are included in the
-- index. If a file matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the file isn\'t included in the
-- index.
slackConfiguration_exclusionPatterns :: Lens.Lens' SlackConfiguration (Prelude.Maybe [Prelude.Text])
slackConfiguration_exclusionPatterns = Lens.lens (\SlackConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@SlackConfiguration' {} a -> s {exclusionPatterns = a} :: SlackConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map Slack data
-- source attributes or field names to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to Slack
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Slack data source field names must exist in your Slack custom
-- metadata.
slackConfiguration_fieldMappings :: Lens.Lens' SlackConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
slackConfiguration_fieldMappings = Lens.lens (\SlackConfiguration' {fieldMappings} -> fieldMappings) (\s@SlackConfiguration' {} a -> s {fieldMappings = a} :: SlackConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain attached files
-- in your Slack workspace team. Files that match the patterns are included
-- in the index. Files that don\'t match the patterns are excluded from the
-- index. If a file matches both an inclusion and exclusion pattern, the
-- exclusion pattern takes precedence and the file isn\'t included in the
-- index.
slackConfiguration_inclusionPatterns :: Lens.Lens' SlackConfiguration (Prelude.Maybe [Prelude.Text])
slackConfiguration_inclusionPatterns = Lens.lens (\SlackConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@SlackConfiguration' {} a -> s {inclusionPatterns = a} :: SlackConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The number of hours for change log to look back from when you last
-- synchronized your data. You can look back up to 7 days or 168 hours.
--
-- Change log updates your index only if new content was added since you
-- last synced your data. Updated or deleted content from before you last
-- synced does not get updated in your index. To capture updated or deleted
-- content before you last synced, set the @LookBackPeriod@ to the number
-- of hours you want change log to look back.
slackConfiguration_lookBackPeriod :: Lens.Lens' SlackConfiguration (Prelude.Maybe Prelude.Natural)
slackConfiguration_lookBackPeriod = Lens.lens (\SlackConfiguration' {lookBackPeriod} -> lookBackPeriod) (\s@SlackConfiguration' {} a -> s {lookBackPeriod = a} :: SlackConfiguration)

-- | The list of private channel names from your Slack workspace team. You
-- use this if you want to index specific private channels, not all private
-- channels. You can also use regular expression patterns to filter private
-- channels.
slackConfiguration_privateChannelFilter :: Lens.Lens' SlackConfiguration (Prelude.Maybe [Prelude.Text])
slackConfiguration_privateChannelFilter = Lens.lens (\SlackConfiguration' {privateChannelFilter} -> privateChannelFilter) (\s@SlackConfiguration' {} a -> s {privateChannelFilter = a} :: SlackConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The list of public channel names to index from your Slack workspace
-- team. You use this if you want to index specific public channels, not
-- all public channels. You can also use regular expression patterns to
-- filter public channels.
slackConfiguration_publicChannelFilter :: Lens.Lens' SlackConfiguration (Prelude.Maybe [Prelude.Text])
slackConfiguration_publicChannelFilter = Lens.lens (\SlackConfiguration' {publicChannelFilter} -> publicChannelFilter) (\s@SlackConfiguration' {} a -> s {publicChannelFilter = a} :: SlackConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to use the Slack change log to determine which documents require
-- updating in the index. Depending on the Slack change log\'s size, it may
-- take longer for Amazon Kendra to use the change log than to scan all of
-- your documents in Slack.
slackConfiguration_useChangeLog :: Lens.Lens' SlackConfiguration (Prelude.Maybe Prelude.Bool)
slackConfiguration_useChangeLog = Lens.lens (\SlackConfiguration' {useChangeLog} -> useChangeLog) (\s@SlackConfiguration' {} a -> s {useChangeLog = a} :: SlackConfiguration)

-- | Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Slack. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
slackConfiguration_vpcConfiguration :: Lens.Lens' SlackConfiguration (Prelude.Maybe DataSourceVpcConfiguration)
slackConfiguration_vpcConfiguration = Lens.lens (\SlackConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@SlackConfiguration' {} a -> s {vpcConfiguration = a} :: SlackConfiguration)

-- | The identifier of the team in the Slack workspace. For example,
-- /T0123456789/.
--
-- You can find your team ID in the URL of the main page of your Slack
-- workspace. When you log in to Slack via a browser, you are directed to
-- the URL of the main page. For example,
-- /https:\/\/app.slack.com\/client\/__T0123456789__\/.../.
slackConfiguration_teamId :: Lens.Lens' SlackConfiguration Prelude.Text
slackConfiguration_teamId = Lens.lens (\SlackConfiguration' {teamId} -> teamId) (\s@SlackConfiguration' {} a -> s {teamId = a} :: SlackConfiguration)

-- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your Slack workspace
-- team. The secret must contain a JSON structure with the following keys:
--
-- -   slackToken—The user or bot token created in Slack. For more
--     information on creating a token in Slack, see
--     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-slack.html#slack-authentication Authentication for a Slack data source>.
slackConfiguration_secretArn :: Lens.Lens' SlackConfiguration Prelude.Text
slackConfiguration_secretArn = Lens.lens (\SlackConfiguration' {secretArn} -> secretArn) (\s@SlackConfiguration' {} a -> s {secretArn = a} :: SlackConfiguration)

-- | Specify whether to index public channels, private channels, group
-- messages, and direct messages. You can specify one or more of these
-- options.
slackConfiguration_slackEntityList :: Lens.Lens' SlackConfiguration (Prelude.NonEmpty SlackEntity)
slackConfiguration_slackEntityList = Lens.lens (\SlackConfiguration' {slackEntityList} -> slackEntityList) (\s@SlackConfiguration' {} a -> s {slackEntityList = a} :: SlackConfiguration) Prelude.. Lens.coerced

-- | The date to start crawling your data from your Slack workspace team. The
-- date must follow this format: @yyyy-mm-dd@.
slackConfiguration_sinceCrawlDate :: Lens.Lens' SlackConfiguration Prelude.Text
slackConfiguration_sinceCrawlDate = Lens.lens (\SlackConfiguration' {sinceCrawlDate} -> sinceCrawlDate) (\s@SlackConfiguration' {} a -> s {sinceCrawlDate = a} :: SlackConfiguration)

instance Data.FromJSON SlackConfiguration where
  parseJSON =
    Data.withObject
      "SlackConfiguration"
      ( \x ->
          SlackConfiguration'
            Prelude.<$> (x Data..:? "CrawlBotMessage")
            Prelude.<*> (x Data..:? "ExcludeArchived")
            Prelude.<*> ( x
                            Data..:? "ExclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FieldMappings")
            Prelude.<*> ( x
                            Data..:? "InclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LookBackPeriod")
            Prelude.<*> ( x
                            Data..:? "PrivateChannelFilter"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "PublicChannelFilter"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UseChangeLog")
            Prelude.<*> (x Data..:? "VpcConfiguration")
            Prelude.<*> (x Data..: "TeamId")
            Prelude.<*> (x Data..: "SecretArn")
            Prelude.<*> (x Data..: "SlackEntityList")
            Prelude.<*> (x Data..: "SinceCrawlDate")
      )

instance Prelude.Hashable SlackConfiguration where
  hashWithSalt _salt SlackConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` crawlBotMessage
      `Prelude.hashWithSalt` excludeArchived
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` fieldMappings
      `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` lookBackPeriod
      `Prelude.hashWithSalt` privateChannelFilter
      `Prelude.hashWithSalt` publicChannelFilter
      `Prelude.hashWithSalt` useChangeLog
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` teamId
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` slackEntityList
      `Prelude.hashWithSalt` sinceCrawlDate

instance Prelude.NFData SlackConfiguration where
  rnf SlackConfiguration' {..} =
    Prelude.rnf crawlBotMessage `Prelude.seq`
      Prelude.rnf excludeArchived `Prelude.seq`
        Prelude.rnf exclusionPatterns `Prelude.seq`
          Prelude.rnf fieldMappings `Prelude.seq`
            Prelude.rnf inclusionPatterns `Prelude.seq`
              Prelude.rnf lookBackPeriod `Prelude.seq`
                Prelude.rnf privateChannelFilter `Prelude.seq`
                  Prelude.rnf publicChannelFilter `Prelude.seq`
                    Prelude.rnf useChangeLog `Prelude.seq`
                      Prelude.rnf vpcConfiguration `Prelude.seq`
                        Prelude.rnf teamId `Prelude.seq`
                          Prelude.rnf secretArn `Prelude.seq`
                            Prelude.rnf slackEntityList `Prelude.seq`
                              Prelude.rnf sinceCrawlDate

instance Data.ToJSON SlackConfiguration where
  toJSON SlackConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CrawlBotMessage" Data..=)
              Prelude.<$> crawlBotMessage,
            ("ExcludeArchived" Data..=)
              Prelude.<$> excludeArchived,
            ("ExclusionPatterns" Data..=)
              Prelude.<$> exclusionPatterns,
            ("FieldMappings" Data..=) Prelude.<$> fieldMappings,
            ("InclusionPatterns" Data..=)
              Prelude.<$> inclusionPatterns,
            ("LookBackPeriod" Data..=)
              Prelude.<$> lookBackPeriod,
            ("PrivateChannelFilter" Data..=)
              Prelude.<$> privateChannelFilter,
            ("PublicChannelFilter" Data..=)
              Prelude.<$> publicChannelFilter,
            ("UseChangeLog" Data..=) Prelude.<$> useChangeLog,
            ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            Prelude.Just ("TeamId" Data..= teamId),
            Prelude.Just ("SecretArn" Data..= secretArn),
            Prelude.Just
              ("SlackEntityList" Data..= slackEntityList),
            Prelude.Just
              ("SinceCrawlDate" Data..= sinceCrawlDate)
          ]
      )
