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
-- Module      : Amazonka.MacieV2.GetMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an account that\'s associated with an Amazon
-- Macie administrator account.
module Amazonka.MacieV2.GetMember
  ( -- * Creating a Request
    GetMember (..),
    newGetMember,

    -- * Request Lenses
    getMember_id,

    -- * Destructuring the Response
    GetMemberResponse (..),
    newGetMemberResponse,

    -- * Response Lenses
    getMemberResponse_tags,
    getMemberResponse_email,
    getMemberResponse_arn,
    getMemberResponse_masterAccountId,
    getMemberResponse_accountId,
    getMemberResponse_invitedAt,
    getMemberResponse_administratorAccountId,
    getMemberResponse_relationshipStatus,
    getMemberResponse_updatedAt,
    getMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMember' smart constructor.
data GetMember = GetMember'
  { -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getMember_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newGetMember ::
  -- | 'id'
  Prelude.Text ->
  GetMember
newGetMember pId_ = GetMember' {id = pId_}

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
getMember_id :: Lens.Lens' GetMember Prelude.Text
getMember_id = Lens.lens (\GetMember' {id} -> id) (\s@GetMember' {} a -> s {id = a} :: GetMember)

instance Core.AWSRequest GetMember where
  type AWSResponse GetMember = GetMemberResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMemberResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "email")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "masterAccountId")
            Prelude.<*> (x Core..?> "accountId")
            Prelude.<*> (x Core..?> "invitedAt")
            Prelude.<*> (x Core..?> "administratorAccountId")
            Prelude.<*> (x Core..?> "relationshipStatus")
            Prelude.<*> (x Core..?> "updatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMember where
  hashWithSalt _salt GetMember' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetMember where
  rnf GetMember' {..} = Prelude.rnf id

instance Core.ToHeaders GetMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetMember where
  toPath GetMember' {..} =
    Prelude.mconcat ["/members/", Core.toBS id]

instance Core.ToQuery GetMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMemberResponse' smart constructor.
data GetMemberResponse = GetMemberResponse'
  { -- | A map of key-value pairs that specifies which tags (keys and values) are
    -- associated with the account in Amazon Macie.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The email address for the account.
    email :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | (Deprecated) The Amazon Web Services account ID for the administrator
    -- account. This property has been replaced by the administratorAccountId
    -- property and is retained only for backward compatibility.
    masterAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID for the account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when an Amazon
    -- Macie membership invitation was last sent to the account. This value is
    -- null if an invitation hasn\'t been sent to the account.
    invitedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services account ID for the administrator account.
    administratorAccountId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the relationship between the account and the
    -- administrator account.
    relationshipStatus :: Prelude.Maybe RelationshipStatus,
    -- | The date and time, in UTC and extended ISO 8601 format, of the most
    -- recent change to the status of the relationship between the account and
    -- the administrator account.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getMemberResponse_tags' - A map of key-value pairs that specifies which tags (keys and values) are
-- associated with the account in Amazon Macie.
--
-- 'email', 'getMemberResponse_email' - The email address for the account.
--
-- 'arn', 'getMemberResponse_arn' - The Amazon Resource Name (ARN) of the account.
--
-- 'masterAccountId', 'getMemberResponse_masterAccountId' - (Deprecated) The Amazon Web Services account ID for the administrator
-- account. This property has been replaced by the administratorAccountId
-- property and is retained only for backward compatibility.
--
-- 'accountId', 'getMemberResponse_accountId' - The Amazon Web Services account ID for the account.
--
-- 'invitedAt', 'getMemberResponse_invitedAt' - The date and time, in UTC and extended ISO 8601 format, when an Amazon
-- Macie membership invitation was last sent to the account. This value is
-- null if an invitation hasn\'t been sent to the account.
--
-- 'administratorAccountId', 'getMemberResponse_administratorAccountId' - The Amazon Web Services account ID for the administrator account.
--
-- 'relationshipStatus', 'getMemberResponse_relationshipStatus' - The current status of the relationship between the account and the
-- administrator account.
--
-- 'updatedAt', 'getMemberResponse_updatedAt' - The date and time, in UTC and extended ISO 8601 format, of the most
-- recent change to the status of the relationship between the account and
-- the administrator account.
--
-- 'httpStatus', 'getMemberResponse_httpStatus' - The response's http status code.
newGetMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMemberResponse
newGetMemberResponse pHttpStatus_ =
  GetMemberResponse'
    { tags = Prelude.Nothing,
      email = Prelude.Nothing,
      arn = Prelude.Nothing,
      masterAccountId = Prelude.Nothing,
      accountId = Prelude.Nothing,
      invitedAt = Prelude.Nothing,
      administratorAccountId = Prelude.Nothing,
      relationshipStatus = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of key-value pairs that specifies which tags (keys and values) are
-- associated with the account in Amazon Macie.
getMemberResponse_tags :: Lens.Lens' GetMemberResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getMemberResponse_tags = Lens.lens (\GetMemberResponse' {tags} -> tags) (\s@GetMemberResponse' {} a -> s {tags = a} :: GetMemberResponse) Prelude.. Lens.mapping Lens.coerced

-- | The email address for the account.
getMemberResponse_email :: Lens.Lens' GetMemberResponse (Prelude.Maybe Prelude.Text)
getMemberResponse_email = Lens.lens (\GetMemberResponse' {email} -> email) (\s@GetMemberResponse' {} a -> s {email = a} :: GetMemberResponse)

-- | The Amazon Resource Name (ARN) of the account.
getMemberResponse_arn :: Lens.Lens' GetMemberResponse (Prelude.Maybe Prelude.Text)
getMemberResponse_arn = Lens.lens (\GetMemberResponse' {arn} -> arn) (\s@GetMemberResponse' {} a -> s {arn = a} :: GetMemberResponse)

-- | (Deprecated) The Amazon Web Services account ID for the administrator
-- account. This property has been replaced by the administratorAccountId
-- property and is retained only for backward compatibility.
getMemberResponse_masterAccountId :: Lens.Lens' GetMemberResponse (Prelude.Maybe Prelude.Text)
getMemberResponse_masterAccountId = Lens.lens (\GetMemberResponse' {masterAccountId} -> masterAccountId) (\s@GetMemberResponse' {} a -> s {masterAccountId = a} :: GetMemberResponse)

-- | The Amazon Web Services account ID for the account.
getMemberResponse_accountId :: Lens.Lens' GetMemberResponse (Prelude.Maybe Prelude.Text)
getMemberResponse_accountId = Lens.lens (\GetMemberResponse' {accountId} -> accountId) (\s@GetMemberResponse' {} a -> s {accountId = a} :: GetMemberResponse)

-- | The date and time, in UTC and extended ISO 8601 format, when an Amazon
-- Macie membership invitation was last sent to the account. This value is
-- null if an invitation hasn\'t been sent to the account.
getMemberResponse_invitedAt :: Lens.Lens' GetMemberResponse (Prelude.Maybe Prelude.UTCTime)
getMemberResponse_invitedAt = Lens.lens (\GetMemberResponse' {invitedAt} -> invitedAt) (\s@GetMemberResponse' {} a -> s {invitedAt = a} :: GetMemberResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services account ID for the administrator account.
getMemberResponse_administratorAccountId :: Lens.Lens' GetMemberResponse (Prelude.Maybe Prelude.Text)
getMemberResponse_administratorAccountId = Lens.lens (\GetMemberResponse' {administratorAccountId} -> administratorAccountId) (\s@GetMemberResponse' {} a -> s {administratorAccountId = a} :: GetMemberResponse)

-- | The current status of the relationship between the account and the
-- administrator account.
getMemberResponse_relationshipStatus :: Lens.Lens' GetMemberResponse (Prelude.Maybe RelationshipStatus)
getMemberResponse_relationshipStatus = Lens.lens (\GetMemberResponse' {relationshipStatus} -> relationshipStatus) (\s@GetMemberResponse' {} a -> s {relationshipStatus = a} :: GetMemberResponse)

-- | The date and time, in UTC and extended ISO 8601 format, of the most
-- recent change to the status of the relationship between the account and
-- the administrator account.
getMemberResponse_updatedAt :: Lens.Lens' GetMemberResponse (Prelude.Maybe Prelude.UTCTime)
getMemberResponse_updatedAt = Lens.lens (\GetMemberResponse' {updatedAt} -> updatedAt) (\s@GetMemberResponse' {} a -> s {updatedAt = a} :: GetMemberResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getMemberResponse_httpStatus :: Lens.Lens' GetMemberResponse Prelude.Int
getMemberResponse_httpStatus = Lens.lens (\GetMemberResponse' {httpStatus} -> httpStatus) (\s@GetMemberResponse' {} a -> s {httpStatus = a} :: GetMemberResponse)

instance Prelude.NFData GetMemberResponse where
  rnf GetMemberResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf masterAccountId
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf invitedAt
      `Prelude.seq` Prelude.rnf administratorAccountId
      `Prelude.seq` Prelude.rnf relationshipStatus
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
