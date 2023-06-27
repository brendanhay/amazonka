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
-- Module      : Amazonka.Backup.UpdateFramework
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing framework identified by its @FrameworkName@ with the
-- input document in JSON format.
module Amazonka.Backup.UpdateFramework
  ( -- * Creating a Request
    UpdateFramework (..),
    newUpdateFramework,

    -- * Request Lenses
    updateFramework_frameworkControls,
    updateFramework_frameworkDescription,
    updateFramework_idempotencyToken,
    updateFramework_frameworkName,

    -- * Destructuring the Response
    UpdateFrameworkResponse (..),
    newUpdateFrameworkResponse,

    -- * Response Lenses
    updateFrameworkResponse_creationTime,
    updateFrameworkResponse_frameworkArn,
    updateFrameworkResponse_frameworkName,
    updateFrameworkResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFramework' smart constructor.
data UpdateFramework = UpdateFramework'
  { -- | A list of the controls that make up the framework. Each control in the
    -- list has a name, input parameters, and scope.
    frameworkControls :: Prelude.Maybe [FrameworkControl],
    -- | An optional description of the framework with a maximum 1,024
    -- characters.
    frameworkDescription :: Prelude.Maybe Prelude.Text,
    -- | A customer-chosen string that you can use to distinguish between
    -- otherwise identical calls to @UpdateFrameworkInput@. Retrying a
    -- successful request with the same idempotency token results in a success
    -- message with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | The unique name of a framework. This name is between 1 and 256
    -- characters, starting with a letter, and consisting of letters (a-z,
    -- A-Z), numbers (0-9), and underscores (_).
    frameworkName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFramework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworkControls', 'updateFramework_frameworkControls' - A list of the controls that make up the framework. Each control in the
-- list has a name, input parameters, and scope.
--
-- 'frameworkDescription', 'updateFramework_frameworkDescription' - An optional description of the framework with a maximum 1,024
-- characters.
--
-- 'idempotencyToken', 'updateFramework_idempotencyToken' - A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @UpdateFrameworkInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
--
-- 'frameworkName', 'updateFramework_frameworkName' - The unique name of a framework. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
newUpdateFramework ::
  -- | 'frameworkName'
  Prelude.Text ->
  UpdateFramework
newUpdateFramework pFrameworkName_ =
  UpdateFramework'
    { frameworkControls =
        Prelude.Nothing,
      frameworkDescription = Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      frameworkName = pFrameworkName_
    }

-- | A list of the controls that make up the framework. Each control in the
-- list has a name, input parameters, and scope.
updateFramework_frameworkControls :: Lens.Lens' UpdateFramework (Prelude.Maybe [FrameworkControl])
updateFramework_frameworkControls = Lens.lens (\UpdateFramework' {frameworkControls} -> frameworkControls) (\s@UpdateFramework' {} a -> s {frameworkControls = a} :: UpdateFramework) Prelude.. Lens.mapping Lens.coerced

-- | An optional description of the framework with a maximum 1,024
-- characters.
updateFramework_frameworkDescription :: Lens.Lens' UpdateFramework (Prelude.Maybe Prelude.Text)
updateFramework_frameworkDescription = Lens.lens (\UpdateFramework' {frameworkDescription} -> frameworkDescription) (\s@UpdateFramework' {} a -> s {frameworkDescription = a} :: UpdateFramework)

-- | A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @UpdateFrameworkInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
updateFramework_idempotencyToken :: Lens.Lens' UpdateFramework (Prelude.Maybe Prelude.Text)
updateFramework_idempotencyToken = Lens.lens (\UpdateFramework' {idempotencyToken} -> idempotencyToken) (\s@UpdateFramework' {} a -> s {idempotencyToken = a} :: UpdateFramework)

-- | The unique name of a framework. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
updateFramework_frameworkName :: Lens.Lens' UpdateFramework Prelude.Text
updateFramework_frameworkName = Lens.lens (\UpdateFramework' {frameworkName} -> frameworkName) (\s@UpdateFramework' {} a -> s {frameworkName = a} :: UpdateFramework)

instance Core.AWSRequest UpdateFramework where
  type
    AWSResponse UpdateFramework =
      UpdateFrameworkResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFrameworkResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "FrameworkArn")
            Prelude.<*> (x Data..?> "FrameworkName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFramework where
  hashWithSalt _salt UpdateFramework' {..} =
    _salt
      `Prelude.hashWithSalt` frameworkControls
      `Prelude.hashWithSalt` frameworkDescription
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` frameworkName

instance Prelude.NFData UpdateFramework where
  rnf UpdateFramework' {..} =
    Prelude.rnf frameworkControls
      `Prelude.seq` Prelude.rnf frameworkDescription
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf frameworkName

instance Data.ToHeaders UpdateFramework where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFramework where
  toJSON UpdateFramework' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FrameworkControls" Data..=)
              Prelude.<$> frameworkControls,
            ("FrameworkDescription" Data..=)
              Prelude.<$> frameworkDescription,
            ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken
          ]
      )

instance Data.ToPath UpdateFramework where
  toPath UpdateFramework' {..} =
    Prelude.mconcat
      ["/audit/frameworks/", Data.toBS frameworkName]

instance Data.ToQuery UpdateFramework where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFrameworkResponse' smart constructor.
data UpdateFrameworkResponse = UpdateFrameworkResponse'
  { -- | The date and time that a framework is created, in ISO 8601
    -- representation. The value of @CreationTime@ is accurate to milliseconds.
    -- For example, 2020-07-10T15:00:00.000-08:00 represents the 10th of July
    -- 2020 at 3:00 PM 8 hours behind UTC.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    frameworkArn :: Prelude.Maybe Prelude.Text,
    -- | The unique name of a framework. This name is between 1 and 256
    -- characters, starting with a letter, and consisting of letters (a-z,
    -- A-Z), numbers (0-9), and underscores (_).
    frameworkName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFrameworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'updateFrameworkResponse_creationTime' - The date and time that a framework is created, in ISO 8601
-- representation. The value of @CreationTime@ is accurate to milliseconds.
-- For example, 2020-07-10T15:00:00.000-08:00 represents the 10th of July
-- 2020 at 3:00 PM 8 hours behind UTC.
--
-- 'frameworkArn', 'updateFrameworkResponse_frameworkArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'frameworkName', 'updateFrameworkResponse_frameworkName' - The unique name of a framework. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
--
-- 'httpStatus', 'updateFrameworkResponse_httpStatus' - The response's http status code.
newUpdateFrameworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFrameworkResponse
newUpdateFrameworkResponse pHttpStatus_ =
  UpdateFrameworkResponse'
    { creationTime =
        Prelude.Nothing,
      frameworkArn = Prelude.Nothing,
      frameworkName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that a framework is created, in ISO 8601
-- representation. The value of @CreationTime@ is accurate to milliseconds.
-- For example, 2020-07-10T15:00:00.000-08:00 represents the 10th of July
-- 2020 at 3:00 PM 8 hours behind UTC.
updateFrameworkResponse_creationTime :: Lens.Lens' UpdateFrameworkResponse (Prelude.Maybe Prelude.UTCTime)
updateFrameworkResponse_creationTime = Lens.lens (\UpdateFrameworkResponse' {creationTime} -> creationTime) (\s@UpdateFrameworkResponse' {} a -> s {creationTime = a} :: UpdateFrameworkResponse) Prelude.. Lens.mapping Data._Time

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
updateFrameworkResponse_frameworkArn :: Lens.Lens' UpdateFrameworkResponse (Prelude.Maybe Prelude.Text)
updateFrameworkResponse_frameworkArn = Lens.lens (\UpdateFrameworkResponse' {frameworkArn} -> frameworkArn) (\s@UpdateFrameworkResponse' {} a -> s {frameworkArn = a} :: UpdateFrameworkResponse)

-- | The unique name of a framework. This name is between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
updateFrameworkResponse_frameworkName :: Lens.Lens' UpdateFrameworkResponse (Prelude.Maybe Prelude.Text)
updateFrameworkResponse_frameworkName = Lens.lens (\UpdateFrameworkResponse' {frameworkName} -> frameworkName) (\s@UpdateFrameworkResponse' {} a -> s {frameworkName = a} :: UpdateFrameworkResponse)

-- | The response's http status code.
updateFrameworkResponse_httpStatus :: Lens.Lens' UpdateFrameworkResponse Prelude.Int
updateFrameworkResponse_httpStatus = Lens.lens (\UpdateFrameworkResponse' {httpStatus} -> httpStatus) (\s@UpdateFrameworkResponse' {} a -> s {httpStatus = a} :: UpdateFrameworkResponse)

instance Prelude.NFData UpdateFrameworkResponse where
  rnf UpdateFrameworkResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf frameworkArn
      `Prelude.seq` Prelude.rnf frameworkName
      `Prelude.seq` Prelude.rnf httpStatus
