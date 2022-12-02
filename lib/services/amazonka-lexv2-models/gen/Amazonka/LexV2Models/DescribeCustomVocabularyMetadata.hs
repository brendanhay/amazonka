{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.DescribeCustomVocabularyMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides metadata information about a custom vocabulary.
module Amazonka.LexV2Models.DescribeCustomVocabularyMetadata
  ( -- * Creating a Request
    DescribeCustomVocabularyMetadata (..),
    newDescribeCustomVocabularyMetadata,

    -- * Request Lenses
    describeCustomVocabularyMetadata_botId,
    describeCustomVocabularyMetadata_botVersion,
    describeCustomVocabularyMetadata_localeId,

    -- * Destructuring the Response
    DescribeCustomVocabularyMetadataResponse (..),
    newDescribeCustomVocabularyMetadataResponse,

    -- * Response Lenses
    describeCustomVocabularyMetadataResponse_customVocabularyStatus,
    describeCustomVocabularyMetadataResponse_botVersion,
    describeCustomVocabularyMetadataResponse_creationDateTime,
    describeCustomVocabularyMetadataResponse_localeId,
    describeCustomVocabularyMetadataResponse_botId,
    describeCustomVocabularyMetadataResponse_lastUpdatedDateTime,
    describeCustomVocabularyMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomVocabularyMetadata' smart constructor.
data DescribeCustomVocabularyMetadata = DescribeCustomVocabularyMetadata'
  { -- | The unique identifier of the bot that contains the custom vocabulary.
    botId :: Prelude.Text,
    -- | The bot version of the bot to return metadata for.
    botVersion :: Prelude.Text,
    -- | The locale to return the custom vocabulary information for. The locale
    -- must be @en_GB@.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomVocabularyMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'describeCustomVocabularyMetadata_botId' - The unique identifier of the bot that contains the custom vocabulary.
--
-- 'botVersion', 'describeCustomVocabularyMetadata_botVersion' - The bot version of the bot to return metadata for.
--
-- 'localeId', 'describeCustomVocabularyMetadata_localeId' - The locale to return the custom vocabulary information for. The locale
-- must be @en_GB@.
newDescribeCustomVocabularyMetadata ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  DescribeCustomVocabularyMetadata
newDescribeCustomVocabularyMetadata
  pBotId_
  pBotVersion_
  pLocaleId_ =
    DescribeCustomVocabularyMetadata'
      { botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The unique identifier of the bot that contains the custom vocabulary.
describeCustomVocabularyMetadata_botId :: Lens.Lens' DescribeCustomVocabularyMetadata Prelude.Text
describeCustomVocabularyMetadata_botId = Lens.lens (\DescribeCustomVocabularyMetadata' {botId} -> botId) (\s@DescribeCustomVocabularyMetadata' {} a -> s {botId = a} :: DescribeCustomVocabularyMetadata)

-- | The bot version of the bot to return metadata for.
describeCustomVocabularyMetadata_botVersion :: Lens.Lens' DescribeCustomVocabularyMetadata Prelude.Text
describeCustomVocabularyMetadata_botVersion = Lens.lens (\DescribeCustomVocabularyMetadata' {botVersion} -> botVersion) (\s@DescribeCustomVocabularyMetadata' {} a -> s {botVersion = a} :: DescribeCustomVocabularyMetadata)

-- | The locale to return the custom vocabulary information for. The locale
-- must be @en_GB@.
describeCustomVocabularyMetadata_localeId :: Lens.Lens' DescribeCustomVocabularyMetadata Prelude.Text
describeCustomVocabularyMetadata_localeId = Lens.lens (\DescribeCustomVocabularyMetadata' {localeId} -> localeId) (\s@DescribeCustomVocabularyMetadata' {} a -> s {localeId = a} :: DescribeCustomVocabularyMetadata)

instance
  Core.AWSRequest
    DescribeCustomVocabularyMetadata
  where
  type
    AWSResponse DescribeCustomVocabularyMetadata =
      DescribeCustomVocabularyMetadataResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomVocabularyMetadataResponse'
            Prelude.<$> (x Data..?> "customVocabularyStatus")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCustomVocabularyMetadata
  where
  hashWithSalt
    _salt
    DescribeCustomVocabularyMetadata' {..} =
      _salt `Prelude.hashWithSalt` botId
        `Prelude.hashWithSalt` botVersion
        `Prelude.hashWithSalt` localeId

instance
  Prelude.NFData
    DescribeCustomVocabularyMetadata
  where
  rnf DescribeCustomVocabularyMetadata' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance
  Data.ToHeaders
    DescribeCustomVocabularyMetadata
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeCustomVocabularyMetadata where
  toPath DescribeCustomVocabularyMetadata' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/customvocabulary/DEFAULT/metadata"
      ]

instance
  Data.ToQuery
    DescribeCustomVocabularyMetadata
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomVocabularyMetadataResponse' smart constructor.
data DescribeCustomVocabularyMetadataResponse = DescribeCustomVocabularyMetadataResponse'
  { -- | The status of the custom vocabulary. If the status is @Ready@ the custom
    -- vocabulary is ready to use.
    customVocabularyStatus :: Prelude.Maybe CustomVocabularyStatus,
    -- | The version of the bot that contains the custom vocabulary to describe.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the custom vocabulary was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The locale that contains the custom vocabulary to describe.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that contains the custom vocabulary.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the custom vocabulary was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomVocabularyMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customVocabularyStatus', 'describeCustomVocabularyMetadataResponse_customVocabularyStatus' - The status of the custom vocabulary. If the status is @Ready@ the custom
-- vocabulary is ready to use.
--
-- 'botVersion', 'describeCustomVocabularyMetadataResponse_botVersion' - The version of the bot that contains the custom vocabulary to describe.
--
-- 'creationDateTime', 'describeCustomVocabularyMetadataResponse_creationDateTime' - The date and time that the custom vocabulary was created.
--
-- 'localeId', 'describeCustomVocabularyMetadataResponse_localeId' - The locale that contains the custom vocabulary to describe.
--
-- 'botId', 'describeCustomVocabularyMetadataResponse_botId' - The identifier of the bot that contains the custom vocabulary.
--
-- 'lastUpdatedDateTime', 'describeCustomVocabularyMetadataResponse_lastUpdatedDateTime' - The date and time that the custom vocabulary was last updated.
--
-- 'httpStatus', 'describeCustomVocabularyMetadataResponse_httpStatus' - The response's http status code.
newDescribeCustomVocabularyMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomVocabularyMetadataResponse
newDescribeCustomVocabularyMetadataResponse
  pHttpStatus_ =
    DescribeCustomVocabularyMetadataResponse'
      { customVocabularyStatus =
          Prelude.Nothing,
        botVersion = Prelude.Nothing,
        creationDateTime =
          Prelude.Nothing,
        localeId = Prelude.Nothing,
        botId = Prelude.Nothing,
        lastUpdatedDateTime =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the custom vocabulary. If the status is @Ready@ the custom
-- vocabulary is ready to use.
describeCustomVocabularyMetadataResponse_customVocabularyStatus :: Lens.Lens' DescribeCustomVocabularyMetadataResponse (Prelude.Maybe CustomVocabularyStatus)
describeCustomVocabularyMetadataResponse_customVocabularyStatus = Lens.lens (\DescribeCustomVocabularyMetadataResponse' {customVocabularyStatus} -> customVocabularyStatus) (\s@DescribeCustomVocabularyMetadataResponse' {} a -> s {customVocabularyStatus = a} :: DescribeCustomVocabularyMetadataResponse)

-- | The version of the bot that contains the custom vocabulary to describe.
describeCustomVocabularyMetadataResponse_botVersion :: Lens.Lens' DescribeCustomVocabularyMetadataResponse (Prelude.Maybe Prelude.Text)
describeCustomVocabularyMetadataResponse_botVersion = Lens.lens (\DescribeCustomVocabularyMetadataResponse' {botVersion} -> botVersion) (\s@DescribeCustomVocabularyMetadataResponse' {} a -> s {botVersion = a} :: DescribeCustomVocabularyMetadataResponse)

-- | The date and time that the custom vocabulary was created.
describeCustomVocabularyMetadataResponse_creationDateTime :: Lens.Lens' DescribeCustomVocabularyMetadataResponse (Prelude.Maybe Prelude.UTCTime)
describeCustomVocabularyMetadataResponse_creationDateTime = Lens.lens (\DescribeCustomVocabularyMetadataResponse' {creationDateTime} -> creationDateTime) (\s@DescribeCustomVocabularyMetadataResponse' {} a -> s {creationDateTime = a} :: DescribeCustomVocabularyMetadataResponse) Prelude.. Lens.mapping Data._Time

-- | The locale that contains the custom vocabulary to describe.
describeCustomVocabularyMetadataResponse_localeId :: Lens.Lens' DescribeCustomVocabularyMetadataResponse (Prelude.Maybe Prelude.Text)
describeCustomVocabularyMetadataResponse_localeId = Lens.lens (\DescribeCustomVocabularyMetadataResponse' {localeId} -> localeId) (\s@DescribeCustomVocabularyMetadataResponse' {} a -> s {localeId = a} :: DescribeCustomVocabularyMetadataResponse)

-- | The identifier of the bot that contains the custom vocabulary.
describeCustomVocabularyMetadataResponse_botId :: Lens.Lens' DescribeCustomVocabularyMetadataResponse (Prelude.Maybe Prelude.Text)
describeCustomVocabularyMetadataResponse_botId = Lens.lens (\DescribeCustomVocabularyMetadataResponse' {botId} -> botId) (\s@DescribeCustomVocabularyMetadataResponse' {} a -> s {botId = a} :: DescribeCustomVocabularyMetadataResponse)

-- | The date and time that the custom vocabulary was last updated.
describeCustomVocabularyMetadataResponse_lastUpdatedDateTime :: Lens.Lens' DescribeCustomVocabularyMetadataResponse (Prelude.Maybe Prelude.UTCTime)
describeCustomVocabularyMetadataResponse_lastUpdatedDateTime = Lens.lens (\DescribeCustomVocabularyMetadataResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeCustomVocabularyMetadataResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeCustomVocabularyMetadataResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeCustomVocabularyMetadataResponse_httpStatus :: Lens.Lens' DescribeCustomVocabularyMetadataResponse Prelude.Int
describeCustomVocabularyMetadataResponse_httpStatus = Lens.lens (\DescribeCustomVocabularyMetadataResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomVocabularyMetadataResponse' {} a -> s {httpStatus = a} :: DescribeCustomVocabularyMetadataResponse)

instance
  Prelude.NFData
    DescribeCustomVocabularyMetadataResponse
  where
  rnf DescribeCustomVocabularyMetadataResponse' {..} =
    Prelude.rnf customVocabularyStatus
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf httpStatus
