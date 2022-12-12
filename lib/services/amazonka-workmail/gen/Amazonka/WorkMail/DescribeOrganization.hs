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
-- Module      : Amazonka.WorkMail.DescribeOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more information regarding a given organization based on its
-- identifier.
module Amazonka.WorkMail.DescribeOrganization
  ( -- * Creating a Request
    DescribeOrganization (..),
    newDescribeOrganization,

    -- * Request Lenses
    describeOrganization_organizationId,

    -- * Destructuring the Response
    DescribeOrganizationResponse (..),
    newDescribeOrganizationResponse,

    -- * Response Lenses
    describeOrganizationResponse_arn,
    describeOrganizationResponse_alias,
    describeOrganizationResponse_completedDate,
    describeOrganizationResponse_defaultMailDomain,
    describeOrganizationResponse_directoryId,
    describeOrganizationResponse_directoryType,
    describeOrganizationResponse_errorMessage,
    describeOrganizationResponse_organizationId,
    describeOrganizationResponse_state,
    describeOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDescribeOrganization' smart constructor.
data DescribeOrganization = DescribeOrganization'
  { -- | The identifier for the organization to be described.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'describeOrganization_organizationId' - The identifier for the organization to be described.
newDescribeOrganization ::
  -- | 'organizationId'
  Prelude.Text ->
  DescribeOrganization
newDescribeOrganization pOrganizationId_ =
  DescribeOrganization'
    { organizationId =
        pOrganizationId_
    }

-- | The identifier for the organization to be described.
describeOrganization_organizationId :: Lens.Lens' DescribeOrganization Prelude.Text
describeOrganization_organizationId = Lens.lens (\DescribeOrganization' {organizationId} -> organizationId) (\s@DescribeOrganization' {} a -> s {organizationId = a} :: DescribeOrganization)

instance Core.AWSRequest DescribeOrganization where
  type
    AWSResponse DescribeOrganization =
      DescribeOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "Alias")
            Prelude.<*> (x Data..?> "CompletedDate")
            Prelude.<*> (x Data..?> "DefaultMailDomain")
            Prelude.<*> (x Data..?> "DirectoryId")
            Prelude.<*> (x Data..?> "DirectoryType")
            Prelude.<*> (x Data..?> "ErrorMessage")
            Prelude.<*> (x Data..?> "OrganizationId")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOrganization where
  hashWithSalt _salt DescribeOrganization' {..} =
    _salt `Prelude.hashWithSalt` organizationId

instance Prelude.NFData DescribeOrganization where
  rnf DescribeOrganization' {..} =
    Prelude.rnf organizationId

instance Data.ToHeaders DescribeOrganization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DescribeOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeOrganization where
  toJSON DescribeOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )

instance Data.ToPath DescribeOrganization where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeOrganization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { -- | The Amazon Resource Name (ARN) of the organization.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The alias for an organization.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The date at which the organization became usable in the WorkMail
    -- context, in UNIX epoch time format.
    completedDate :: Prelude.Maybe Data.POSIX,
    -- | The default mail domain associated with the organization.
    defaultMailDomain :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the directory associated with an WorkMail
    -- organization.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The type of directory associated with the WorkMail organization.
    directoryType :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The error message indicating if unexpected behavior was
    -- encountered with regards to the organization.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The state of an organization.
    state :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeOrganizationResponse_arn' - The Amazon Resource Name (ARN) of the organization.
--
-- 'alias', 'describeOrganizationResponse_alias' - The alias for an organization.
--
-- 'completedDate', 'describeOrganizationResponse_completedDate' - The date at which the organization became usable in the WorkMail
-- context, in UNIX epoch time format.
--
-- 'defaultMailDomain', 'describeOrganizationResponse_defaultMailDomain' - The default mail domain associated with the organization.
--
-- 'directoryId', 'describeOrganizationResponse_directoryId' - The identifier for the directory associated with an WorkMail
-- organization.
--
-- 'directoryType', 'describeOrganizationResponse_directoryType' - The type of directory associated with the WorkMail organization.
--
-- 'errorMessage', 'describeOrganizationResponse_errorMessage' - (Optional) The error message indicating if unexpected behavior was
-- encountered with regards to the organization.
--
-- 'organizationId', 'describeOrganizationResponse_organizationId' - The identifier of an organization.
--
-- 'state', 'describeOrganizationResponse_state' - The state of an organization.
--
-- 'httpStatus', 'describeOrganizationResponse_httpStatus' - The response's http status code.
newDescribeOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationResponse
newDescribeOrganizationResponse pHttpStatus_ =
  DescribeOrganizationResponse'
    { arn =
        Prelude.Nothing,
      alias = Prelude.Nothing,
      completedDate = Prelude.Nothing,
      defaultMailDomain = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      directoryType = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the organization.
describeOrganizationResponse_arn :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Prelude.Text)
describeOrganizationResponse_arn = Lens.lens (\DescribeOrganizationResponse' {arn} -> arn) (\s@DescribeOrganizationResponse' {} a -> s {arn = a} :: DescribeOrganizationResponse)

-- | The alias for an organization.
describeOrganizationResponse_alias :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Prelude.Text)
describeOrganizationResponse_alias = Lens.lens (\DescribeOrganizationResponse' {alias} -> alias) (\s@DescribeOrganizationResponse' {} a -> s {alias = a} :: DescribeOrganizationResponse)

-- | The date at which the organization became usable in the WorkMail
-- context, in UNIX epoch time format.
describeOrganizationResponse_completedDate :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Prelude.UTCTime)
describeOrganizationResponse_completedDate = Lens.lens (\DescribeOrganizationResponse' {completedDate} -> completedDate) (\s@DescribeOrganizationResponse' {} a -> s {completedDate = a} :: DescribeOrganizationResponse) Prelude.. Lens.mapping Data._Time

-- | The default mail domain associated with the organization.
describeOrganizationResponse_defaultMailDomain :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Prelude.Text)
describeOrganizationResponse_defaultMailDomain = Lens.lens (\DescribeOrganizationResponse' {defaultMailDomain} -> defaultMailDomain) (\s@DescribeOrganizationResponse' {} a -> s {defaultMailDomain = a} :: DescribeOrganizationResponse)

-- | The identifier for the directory associated with an WorkMail
-- organization.
describeOrganizationResponse_directoryId :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Prelude.Text)
describeOrganizationResponse_directoryId = Lens.lens (\DescribeOrganizationResponse' {directoryId} -> directoryId) (\s@DescribeOrganizationResponse' {} a -> s {directoryId = a} :: DescribeOrganizationResponse)

-- | The type of directory associated with the WorkMail organization.
describeOrganizationResponse_directoryType :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Prelude.Text)
describeOrganizationResponse_directoryType = Lens.lens (\DescribeOrganizationResponse' {directoryType} -> directoryType) (\s@DescribeOrganizationResponse' {} a -> s {directoryType = a} :: DescribeOrganizationResponse)

-- | (Optional) The error message indicating if unexpected behavior was
-- encountered with regards to the organization.
describeOrganizationResponse_errorMessage :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Prelude.Text)
describeOrganizationResponse_errorMessage = Lens.lens (\DescribeOrganizationResponse' {errorMessage} -> errorMessage) (\s@DescribeOrganizationResponse' {} a -> s {errorMessage = a} :: DescribeOrganizationResponse)

-- | The identifier of an organization.
describeOrganizationResponse_organizationId :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Prelude.Text)
describeOrganizationResponse_organizationId = Lens.lens (\DescribeOrganizationResponse' {organizationId} -> organizationId) (\s@DescribeOrganizationResponse' {} a -> s {organizationId = a} :: DescribeOrganizationResponse)

-- | The state of an organization.
describeOrganizationResponse_state :: Lens.Lens' DescribeOrganizationResponse (Prelude.Maybe Prelude.Text)
describeOrganizationResponse_state = Lens.lens (\DescribeOrganizationResponse' {state} -> state) (\s@DescribeOrganizationResponse' {} a -> s {state = a} :: DescribeOrganizationResponse)

-- | The response's http status code.
describeOrganizationResponse_httpStatus :: Lens.Lens' DescribeOrganizationResponse Prelude.Int
describeOrganizationResponse_httpStatus = Lens.lens (\DescribeOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationResponse)

instance Prelude.NFData DescribeOrganizationResponse where
  rnf DescribeOrganizationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf alias
      `Prelude.seq` Prelude.rnf completedDate
      `Prelude.seq` Prelude.rnf defaultMailDomain
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf directoryType
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
