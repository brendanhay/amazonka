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
-- Module      : Amazonka.CustomerProfiles.CreateDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain, which is a container for all customer data, such as
-- customer profile attributes, object types, profile keys, and encryption
-- keys. You can create multiple domains, and each domain can have multiple
-- third-party integrations.
--
-- Each Amazon Connect instance can be associated with only one domain.
-- Multiple Amazon Connect instances can be associated with one domain.
--
-- Use this API or
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_UpdateDomain.html UpdateDomain>
-- to enable
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_GetMatches.html identity resolution>:
-- set @Matching@ to true.
--
-- To prevent cross-service impersonation when you call this API, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/cross-service-confused-deputy-prevention.html Cross-service confused deputy prevention>
-- for sample policies that you should apply.
module Amazonka.CustomerProfiles.CreateDomain
  ( -- * Creating a Request
    CreateDomain (..),
    newCreateDomain,

    -- * Request Lenses
    createDomain_tags,
    createDomain_matching,
    createDomain_deadLetterQueueUrl,
    createDomain_defaultEncryptionKey,
    createDomain_domainName,
    createDomain_defaultExpirationDays,

    -- * Destructuring the Response
    CreateDomainResponse (..),
    newCreateDomainResponse,

    -- * Response Lenses
    createDomainResponse_tags,
    createDomainResponse_matching,
    createDomainResponse_deadLetterQueueUrl,
    createDomainResponse_defaultEncryptionKey,
    createDomainResponse_httpStatus,
    createDomainResponse_domainName,
    createDomainResponse_defaultExpirationDays,
    createDomainResponse_createdAt,
    createDomainResponse_lastUpdatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    matching :: Prelude.Maybe MatchingRequest,
    -- | The URL of the SQS dead letter queue, which is used for reporting errors
    -- associated with ingesting data from third party applications. You must
    -- set up a policy on the DeadLetterQueue for the SendMessage operation to
    -- enable Amazon Connect Customer Profiles to send messages to the
    -- DeadLetterQueue.
    deadLetterQueueUrl :: Prelude.Maybe Prelude.Text,
    -- | The default encryption key, which is an AWS managed key, is used when no
    -- specific type of encryption key is specified. It is used to encrypt all
    -- data before it is placed in permanent or semi-permanent storage.
    defaultEncryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The default number of days until the data within the domain expires.
    defaultExpirationDays :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDomain_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'matching', 'createDomain_matching' - The process of matching duplicate profiles. If @Matching@ = @true@,
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
-- 'deadLetterQueueUrl', 'createDomain_deadLetterQueueUrl' - The URL of the SQS dead letter queue, which is used for reporting errors
-- associated with ingesting data from third party applications. You must
-- set up a policy on the DeadLetterQueue for the SendMessage operation to
-- enable Amazon Connect Customer Profiles to send messages to the
-- DeadLetterQueue.
--
-- 'defaultEncryptionKey', 'createDomain_defaultEncryptionKey' - The default encryption key, which is an AWS managed key, is used when no
-- specific type of encryption key is specified. It is used to encrypt all
-- data before it is placed in permanent or semi-permanent storage.
--
-- 'domainName', 'createDomain_domainName' - The unique name of the domain.
--
-- 'defaultExpirationDays', 'createDomain_defaultExpirationDays' - The default number of days until the data within the domain expires.
newCreateDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'defaultExpirationDays'
  Prelude.Natural ->
  CreateDomain
newCreateDomain pDomainName_ pDefaultExpirationDays_ =
  CreateDomain'
    { tags = Prelude.Nothing,
      matching = Prelude.Nothing,
      deadLetterQueueUrl = Prelude.Nothing,
      defaultEncryptionKey = Prelude.Nothing,
      domainName = pDomainName_,
      defaultExpirationDays = pDefaultExpirationDays_
    }

-- | The tags used to organize, track, or control access for this resource.
createDomain_tags :: Lens.Lens' CreateDomain (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomain_tags = Lens.lens (\CreateDomain' {tags} -> tags) (\s@CreateDomain' {} a -> s {tags = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

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
createDomain_matching :: Lens.Lens' CreateDomain (Prelude.Maybe MatchingRequest)
createDomain_matching = Lens.lens (\CreateDomain' {matching} -> matching) (\s@CreateDomain' {} a -> s {matching = a} :: CreateDomain)

-- | The URL of the SQS dead letter queue, which is used for reporting errors
-- associated with ingesting data from third party applications. You must
-- set up a policy on the DeadLetterQueue for the SendMessage operation to
-- enable Amazon Connect Customer Profiles to send messages to the
-- DeadLetterQueue.
createDomain_deadLetterQueueUrl :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_deadLetterQueueUrl = Lens.lens (\CreateDomain' {deadLetterQueueUrl} -> deadLetterQueueUrl) (\s@CreateDomain' {} a -> s {deadLetterQueueUrl = a} :: CreateDomain)

-- | The default encryption key, which is an AWS managed key, is used when no
-- specific type of encryption key is specified. It is used to encrypt all
-- data before it is placed in permanent or semi-permanent storage.
createDomain_defaultEncryptionKey :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_defaultEncryptionKey = Lens.lens (\CreateDomain' {defaultEncryptionKey} -> defaultEncryptionKey) (\s@CreateDomain' {} a -> s {defaultEncryptionKey = a} :: CreateDomain)

-- | The unique name of the domain.
createDomain_domainName :: Lens.Lens' CreateDomain Prelude.Text
createDomain_domainName = Lens.lens (\CreateDomain' {domainName} -> domainName) (\s@CreateDomain' {} a -> s {domainName = a} :: CreateDomain)

-- | The default number of days until the data within the domain expires.
createDomain_defaultExpirationDays :: Lens.Lens' CreateDomain Prelude.Natural
createDomain_defaultExpirationDays = Lens.lens (\CreateDomain' {defaultExpirationDays} -> defaultExpirationDays) (\s@CreateDomain' {} a -> s {defaultExpirationDays = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Matching")
            Prelude.<*> (x Data..?> "DeadLetterQueueUrl")
            Prelude.<*> (x Data..?> "DefaultEncryptionKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainName")
            Prelude.<*> (x Data..:> "DefaultExpirationDays")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "LastUpdatedAt")
      )

instance Prelude.Hashable CreateDomain where
  hashWithSalt _salt CreateDomain' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` matching
      `Prelude.hashWithSalt` deadLetterQueueUrl
      `Prelude.hashWithSalt` defaultEncryptionKey
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` defaultExpirationDays

instance Prelude.NFData CreateDomain where
  rnf CreateDomain' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf matching
      `Prelude.seq` Prelude.rnf deadLetterQueueUrl
      `Prelude.seq` Prelude.rnf defaultEncryptionKey
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf defaultExpirationDays

instance Data.ToHeaders CreateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Matching" Data..=) Prelude.<$> matching,
            ("DeadLetterQueueUrl" Data..=)
              Prelude.<$> deadLetterQueueUrl,
            ("DefaultEncryptionKey" Data..=)
              Prelude.<$> defaultEncryptionKey,
            Prelude.Just
              ( "DefaultExpirationDays"
                  Data..= defaultExpirationDays
              )
          ]
      )

instance Data.ToPath CreateDomain where
  toPath CreateDomain' {..} =
    Prelude.mconcat ["/domains/", Data.toBS domainName]

instance Data.ToQuery CreateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    -- | The URL of the SQS dead letter queue, which is used for reporting errors
    -- associated with ingesting data from third party applications.
    deadLetterQueueUrl :: Prelude.Maybe Prelude.Text,
    -- | The default encryption key, which is an AWS managed key, is used when no
    -- specific type of encryption key is specified. It is used to encrypt all
    -- data before it is placed in permanent or semi-permanent storage.
    defaultEncryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The default number of days until the data within the domain expires.
    defaultExpirationDays :: Prelude.Natural,
    -- | The timestamp of when the domain was created.
    createdAt :: Data.POSIX,
    -- | The timestamp of when the domain was most recently edited.
    lastUpdatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDomainResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'matching', 'createDomainResponse_matching' - The process of matching duplicate profiles. If @Matching@ = @true@,
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
-- 'deadLetterQueueUrl', 'createDomainResponse_deadLetterQueueUrl' - The URL of the SQS dead letter queue, which is used for reporting errors
-- associated with ingesting data from third party applications.
--
-- 'defaultEncryptionKey', 'createDomainResponse_defaultEncryptionKey' - The default encryption key, which is an AWS managed key, is used when no
-- specific type of encryption key is specified. It is used to encrypt all
-- data before it is placed in permanent or semi-permanent storage.
--
-- 'httpStatus', 'createDomainResponse_httpStatus' - The response's http status code.
--
-- 'domainName', 'createDomainResponse_domainName' - The unique name of the domain.
--
-- 'defaultExpirationDays', 'createDomainResponse_defaultExpirationDays' - The default number of days until the data within the domain expires.
--
-- 'createdAt', 'createDomainResponse_createdAt' - The timestamp of when the domain was created.
--
-- 'lastUpdatedAt', 'createDomainResponse_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
newCreateDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'defaultExpirationDays'
  Prelude.Natural ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  CreateDomainResponse
newCreateDomainResponse
  pHttpStatus_
  pDomainName_
  pDefaultExpirationDays_
  pCreatedAt_
  pLastUpdatedAt_ =
    CreateDomainResponse'
      { tags = Prelude.Nothing,
        matching = Prelude.Nothing,
        deadLetterQueueUrl = Prelude.Nothing,
        defaultEncryptionKey = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainName = pDomainName_,
        defaultExpirationDays = pDefaultExpirationDays_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_
      }

-- | The tags used to organize, track, or control access for this resource.
createDomainResponse_tags :: Lens.Lens' CreateDomainResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomainResponse_tags = Lens.lens (\CreateDomainResponse' {tags} -> tags) (\s@CreateDomainResponse' {} a -> s {tags = a} :: CreateDomainResponse) Prelude.. Lens.mapping Lens.coerced

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
createDomainResponse_matching :: Lens.Lens' CreateDomainResponse (Prelude.Maybe MatchingResponse)
createDomainResponse_matching = Lens.lens (\CreateDomainResponse' {matching} -> matching) (\s@CreateDomainResponse' {} a -> s {matching = a} :: CreateDomainResponse)

-- | The URL of the SQS dead letter queue, which is used for reporting errors
-- associated with ingesting data from third party applications.
createDomainResponse_deadLetterQueueUrl :: Lens.Lens' CreateDomainResponse (Prelude.Maybe Prelude.Text)
createDomainResponse_deadLetterQueueUrl = Lens.lens (\CreateDomainResponse' {deadLetterQueueUrl} -> deadLetterQueueUrl) (\s@CreateDomainResponse' {} a -> s {deadLetterQueueUrl = a} :: CreateDomainResponse)

-- | The default encryption key, which is an AWS managed key, is used when no
-- specific type of encryption key is specified. It is used to encrypt all
-- data before it is placed in permanent or semi-permanent storage.
createDomainResponse_defaultEncryptionKey :: Lens.Lens' CreateDomainResponse (Prelude.Maybe Prelude.Text)
createDomainResponse_defaultEncryptionKey = Lens.lens (\CreateDomainResponse' {defaultEncryptionKey} -> defaultEncryptionKey) (\s@CreateDomainResponse' {} a -> s {defaultEncryptionKey = a} :: CreateDomainResponse)

-- | The response's http status code.
createDomainResponse_httpStatus :: Lens.Lens' CreateDomainResponse Prelude.Int
createDomainResponse_httpStatus = Lens.lens (\CreateDomainResponse' {httpStatus} -> httpStatus) (\s@CreateDomainResponse' {} a -> s {httpStatus = a} :: CreateDomainResponse)

-- | The unique name of the domain.
createDomainResponse_domainName :: Lens.Lens' CreateDomainResponse Prelude.Text
createDomainResponse_domainName = Lens.lens (\CreateDomainResponse' {domainName} -> domainName) (\s@CreateDomainResponse' {} a -> s {domainName = a} :: CreateDomainResponse)

-- | The default number of days until the data within the domain expires.
createDomainResponse_defaultExpirationDays :: Lens.Lens' CreateDomainResponse Prelude.Natural
createDomainResponse_defaultExpirationDays = Lens.lens (\CreateDomainResponse' {defaultExpirationDays} -> defaultExpirationDays) (\s@CreateDomainResponse' {} a -> s {defaultExpirationDays = a} :: CreateDomainResponse)

-- | The timestamp of when the domain was created.
createDomainResponse_createdAt :: Lens.Lens' CreateDomainResponse Prelude.UTCTime
createDomainResponse_createdAt = Lens.lens (\CreateDomainResponse' {createdAt} -> createdAt) (\s@CreateDomainResponse' {} a -> s {createdAt = a} :: CreateDomainResponse) Prelude.. Data._Time

-- | The timestamp of when the domain was most recently edited.
createDomainResponse_lastUpdatedAt :: Lens.Lens' CreateDomainResponse Prelude.UTCTime
createDomainResponse_lastUpdatedAt = Lens.lens (\CreateDomainResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@CreateDomainResponse' {} a -> s {lastUpdatedAt = a} :: CreateDomainResponse) Prelude.. Data._Time

instance Prelude.NFData CreateDomainResponse where
  rnf CreateDomainResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf matching
      `Prelude.seq` Prelude.rnf deadLetterQueueUrl
      `Prelude.seq` Prelude.rnf defaultEncryptionKey
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf defaultExpirationDays
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
