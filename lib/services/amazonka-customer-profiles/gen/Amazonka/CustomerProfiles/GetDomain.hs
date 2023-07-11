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
-- Module      : Amazonka.CustomerProfiles.GetDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific domain.
module Amazonka.CustomerProfiles.GetDomain
  ( -- * Creating a Request
    GetDomain (..),
    newGetDomain,

    -- * Request Lenses
    getDomain_domainName,

    -- * Destructuring the Response
    GetDomainResponse (..),
    newGetDomainResponse,

    -- * Response Lenses
    getDomainResponse_deadLetterQueueUrl,
    getDomainResponse_defaultEncryptionKey,
    getDomainResponse_defaultExpirationDays,
    getDomainResponse_matching,
    getDomainResponse_stats,
    getDomainResponse_tags,
    getDomainResponse_httpStatus,
    getDomainResponse_domainName,
    getDomainResponse_createdAt,
    getDomainResponse_lastUpdatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDomain' smart constructor.
data GetDomain = GetDomain'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getDomain_domainName' - The unique name of the domain.
newGetDomain ::
  -- | 'domainName'
  Prelude.Text ->
  GetDomain
newGetDomain pDomainName_ =
  GetDomain' {domainName = pDomainName_}

-- | The unique name of the domain.
getDomain_domainName :: Lens.Lens' GetDomain Prelude.Text
getDomain_domainName = Lens.lens (\GetDomain' {domainName} -> domainName) (\s@GetDomain' {} a -> s {domainName = a} :: GetDomain)

instance Core.AWSRequest GetDomain where
  type AWSResponse GetDomain = GetDomainResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainResponse'
            Prelude.<$> (x Data..?> "DeadLetterQueueUrl")
            Prelude.<*> (x Data..?> "DefaultEncryptionKey")
            Prelude.<*> (x Data..?> "DefaultExpirationDays")
            Prelude.<*> (x Data..?> "Matching")
            Prelude.<*> (x Data..?> "Stats")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainName")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "LastUpdatedAt")
      )

instance Prelude.Hashable GetDomain where
  hashWithSalt _salt GetDomain' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetDomain where
  rnf GetDomain' {..} = Prelude.rnf domainName

instance Data.ToHeaders GetDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDomain where
  toPath GetDomain' {..} =
    Prelude.mconcat ["/domains/", Data.toBS domainName]

instance Data.ToQuery GetDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDomainResponse' smart constructor.
data GetDomainResponse = GetDomainResponse'
  { -- | The URL of the SQS dead letter queue, which is used for reporting errors
    -- associated with ingesting data from third party applications.
    deadLetterQueueUrl :: Prelude.Maybe Prelude.Text,
    -- | The default encryption key, which is an AWS managed key, is used when no
    -- specific type of encryption key is specified. It is used to encrypt all
    -- data before it is placed in permanent or semi-permanent storage.
    defaultEncryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The default number of days until the data within the domain expires.
    defaultExpirationDays :: Prelude.Maybe Prelude.Natural,
    -- | The process of matching duplicate profiles. If @Matching@ = @true@,
    -- Amazon Connect Customer Profiles starts a weekly batch process called
    -- Identity Resolution Job. If you do not specify a date and time for
    -- Identity Resolution Job to run, by default it runs every Saturday at
    -- 12AM UTC to detect duplicate profiles in your domains.
    --
    -- After the Identity Resolution Job completes, use the
    -- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_GetMatches.html GetMatches>
    -- API to return and review the results. Or, if you have configured
    -- @ExportingConfig@ in the @MatchingRequest@, you can download the results
    -- from S3.
    matching :: Prelude.Maybe MatchingResponse,
    -- | Usage-specific statistics about the domain.
    stats :: Prelude.Maybe DomainStats,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The timestamp of when the domain was created.
    createdAt :: Data.POSIX,
    -- | The timestamp of when the domain was most recently edited.
    lastUpdatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deadLetterQueueUrl', 'getDomainResponse_deadLetterQueueUrl' - The URL of the SQS dead letter queue, which is used for reporting errors
-- associated with ingesting data from third party applications.
--
-- 'defaultEncryptionKey', 'getDomainResponse_defaultEncryptionKey' - The default encryption key, which is an AWS managed key, is used when no
-- specific type of encryption key is specified. It is used to encrypt all
-- data before it is placed in permanent or semi-permanent storage.
--
-- 'defaultExpirationDays', 'getDomainResponse_defaultExpirationDays' - The default number of days until the data within the domain expires.
--
-- 'matching', 'getDomainResponse_matching' - The process of matching duplicate profiles. If @Matching@ = @true@,
-- Amazon Connect Customer Profiles starts a weekly batch process called
-- Identity Resolution Job. If you do not specify a date and time for
-- Identity Resolution Job to run, by default it runs every Saturday at
-- 12AM UTC to detect duplicate profiles in your domains.
--
-- After the Identity Resolution Job completes, use the
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_GetMatches.html GetMatches>
-- API to return and review the results. Or, if you have configured
-- @ExportingConfig@ in the @MatchingRequest@, you can download the results
-- from S3.
--
-- 'stats', 'getDomainResponse_stats' - Usage-specific statistics about the domain.
--
-- 'tags', 'getDomainResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'getDomainResponse_httpStatus' - The response's http status code.
--
-- 'domainName', 'getDomainResponse_domainName' - The unique name of the domain.
--
-- 'createdAt', 'getDomainResponse_createdAt' - The timestamp of when the domain was created.
--
-- 'lastUpdatedAt', 'getDomainResponse_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
newGetDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  GetDomainResponse
newGetDomainResponse
  pHttpStatus_
  pDomainName_
  pCreatedAt_
  pLastUpdatedAt_ =
    GetDomainResponse'
      { deadLetterQueueUrl =
          Prelude.Nothing,
        defaultEncryptionKey = Prelude.Nothing,
        defaultExpirationDays = Prelude.Nothing,
        matching = Prelude.Nothing,
        stats = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainName = pDomainName_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_
      }

-- | The URL of the SQS dead letter queue, which is used for reporting errors
-- associated with ingesting data from third party applications.
getDomainResponse_deadLetterQueueUrl :: Lens.Lens' GetDomainResponse (Prelude.Maybe Prelude.Text)
getDomainResponse_deadLetterQueueUrl = Lens.lens (\GetDomainResponse' {deadLetterQueueUrl} -> deadLetterQueueUrl) (\s@GetDomainResponse' {} a -> s {deadLetterQueueUrl = a} :: GetDomainResponse)

-- | The default encryption key, which is an AWS managed key, is used when no
-- specific type of encryption key is specified. It is used to encrypt all
-- data before it is placed in permanent or semi-permanent storage.
getDomainResponse_defaultEncryptionKey :: Lens.Lens' GetDomainResponse (Prelude.Maybe Prelude.Text)
getDomainResponse_defaultEncryptionKey = Lens.lens (\GetDomainResponse' {defaultEncryptionKey} -> defaultEncryptionKey) (\s@GetDomainResponse' {} a -> s {defaultEncryptionKey = a} :: GetDomainResponse)

-- | The default number of days until the data within the domain expires.
getDomainResponse_defaultExpirationDays :: Lens.Lens' GetDomainResponse (Prelude.Maybe Prelude.Natural)
getDomainResponse_defaultExpirationDays = Lens.lens (\GetDomainResponse' {defaultExpirationDays} -> defaultExpirationDays) (\s@GetDomainResponse' {} a -> s {defaultExpirationDays = a} :: GetDomainResponse)

-- | The process of matching duplicate profiles. If @Matching@ = @true@,
-- Amazon Connect Customer Profiles starts a weekly batch process called
-- Identity Resolution Job. If you do not specify a date and time for
-- Identity Resolution Job to run, by default it runs every Saturday at
-- 12AM UTC to detect duplicate profiles in your domains.
--
-- After the Identity Resolution Job completes, use the
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_GetMatches.html GetMatches>
-- API to return and review the results. Or, if you have configured
-- @ExportingConfig@ in the @MatchingRequest@, you can download the results
-- from S3.
getDomainResponse_matching :: Lens.Lens' GetDomainResponse (Prelude.Maybe MatchingResponse)
getDomainResponse_matching = Lens.lens (\GetDomainResponse' {matching} -> matching) (\s@GetDomainResponse' {} a -> s {matching = a} :: GetDomainResponse)

-- | Usage-specific statistics about the domain.
getDomainResponse_stats :: Lens.Lens' GetDomainResponse (Prelude.Maybe DomainStats)
getDomainResponse_stats = Lens.lens (\GetDomainResponse' {stats} -> stats) (\s@GetDomainResponse' {} a -> s {stats = a} :: GetDomainResponse)

-- | The tags used to organize, track, or control access for this resource.
getDomainResponse_tags :: Lens.Lens' GetDomainResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDomainResponse_tags = Lens.lens (\GetDomainResponse' {tags} -> tags) (\s@GetDomainResponse' {} a -> s {tags = a} :: GetDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDomainResponse_httpStatus :: Lens.Lens' GetDomainResponse Prelude.Int
getDomainResponse_httpStatus = Lens.lens (\GetDomainResponse' {httpStatus} -> httpStatus) (\s@GetDomainResponse' {} a -> s {httpStatus = a} :: GetDomainResponse)

-- | The unique name of the domain.
getDomainResponse_domainName :: Lens.Lens' GetDomainResponse Prelude.Text
getDomainResponse_domainName = Lens.lens (\GetDomainResponse' {domainName} -> domainName) (\s@GetDomainResponse' {} a -> s {domainName = a} :: GetDomainResponse)

-- | The timestamp of when the domain was created.
getDomainResponse_createdAt :: Lens.Lens' GetDomainResponse Prelude.UTCTime
getDomainResponse_createdAt = Lens.lens (\GetDomainResponse' {createdAt} -> createdAt) (\s@GetDomainResponse' {} a -> s {createdAt = a} :: GetDomainResponse) Prelude.. Data._Time

-- | The timestamp of when the domain was most recently edited.
getDomainResponse_lastUpdatedAt :: Lens.Lens' GetDomainResponse Prelude.UTCTime
getDomainResponse_lastUpdatedAt = Lens.lens (\GetDomainResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetDomainResponse' {} a -> s {lastUpdatedAt = a} :: GetDomainResponse) Prelude.. Data._Time

instance Prelude.NFData GetDomainResponse where
  rnf GetDomainResponse' {..} =
    Prelude.rnf deadLetterQueueUrl
      `Prelude.seq` Prelude.rnf defaultEncryptionKey
      `Prelude.seq` Prelude.rnf defaultExpirationDays
      `Prelude.seq` Prelude.rnf matching
      `Prelude.seq` Prelude.rnf stats
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
