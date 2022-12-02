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
-- Module      : Amazonka.Backup.CreateFramework
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a framework with one or more controls. A framework is a
-- collection of controls that you can use to evaluate your backup
-- practices. By using pre-built customizable controls to define your
-- policies, you can evaluate whether your backup practices comply with
-- your policies and which resources are not yet in compliance.
module Amazonka.Backup.CreateFramework
  ( -- * Creating a Request
    CreateFramework (..),
    newCreateFramework,

    -- * Request Lenses
    createFramework_frameworkDescription,
    createFramework_idempotencyToken,
    createFramework_frameworkTags,
    createFramework_frameworkName,
    createFramework_frameworkControls,

    -- * Destructuring the Response
    CreateFrameworkResponse (..),
    newCreateFrameworkResponse,

    -- * Response Lenses
    createFrameworkResponse_frameworkArn,
    createFrameworkResponse_frameworkName,
    createFrameworkResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFramework' smart constructor.
data CreateFramework = CreateFramework'
  { -- | An optional description of the framework with a maximum of 1,024
    -- characters.
    frameworkDescription :: Prelude.Maybe Prelude.Text,
    -- | A customer-chosen string that you can use to distinguish between
    -- otherwise identical calls to @CreateFrameworkInput@. Retrying a
    -- successful request with the same idempotency token results in a success
    -- message with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Metadata that you can assign to help organize the frameworks that you
    -- create. Each tag is a key-value pair.
    frameworkTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name of the framework. The name must be between 1 and 256
    -- characters, starting with a letter, and consisting of letters (a-z,
    -- A-Z), numbers (0-9), and underscores (_).
    frameworkName :: Prelude.Text,
    -- | A list of the controls that make up the framework. Each control in the
    -- list has a name, input parameters, and scope.
    frameworkControls :: [FrameworkControl]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFramework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworkDescription', 'createFramework_frameworkDescription' - An optional description of the framework with a maximum of 1,024
-- characters.
--
-- 'idempotencyToken', 'createFramework_idempotencyToken' - A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @CreateFrameworkInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
--
-- 'frameworkTags', 'createFramework_frameworkTags' - Metadata that you can assign to help organize the frameworks that you
-- create. Each tag is a key-value pair.
--
-- 'frameworkName', 'createFramework_frameworkName' - The unique name of the framework. The name must be between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
--
-- 'frameworkControls', 'createFramework_frameworkControls' - A list of the controls that make up the framework. Each control in the
-- list has a name, input parameters, and scope.
newCreateFramework ::
  -- | 'frameworkName'
  Prelude.Text ->
  CreateFramework
newCreateFramework pFrameworkName_ =
  CreateFramework'
    { frameworkDescription =
        Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      frameworkTags = Prelude.Nothing,
      frameworkName = pFrameworkName_,
      frameworkControls = Prelude.mempty
    }

-- | An optional description of the framework with a maximum of 1,024
-- characters.
createFramework_frameworkDescription :: Lens.Lens' CreateFramework (Prelude.Maybe Prelude.Text)
createFramework_frameworkDescription = Lens.lens (\CreateFramework' {frameworkDescription} -> frameworkDescription) (\s@CreateFramework' {} a -> s {frameworkDescription = a} :: CreateFramework)

-- | A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @CreateFrameworkInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
createFramework_idempotencyToken :: Lens.Lens' CreateFramework (Prelude.Maybe Prelude.Text)
createFramework_idempotencyToken = Lens.lens (\CreateFramework' {idempotencyToken} -> idempotencyToken) (\s@CreateFramework' {} a -> s {idempotencyToken = a} :: CreateFramework)

-- | Metadata that you can assign to help organize the frameworks that you
-- create. Each tag is a key-value pair.
createFramework_frameworkTags :: Lens.Lens' CreateFramework (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFramework_frameworkTags = Lens.lens (\CreateFramework' {frameworkTags} -> frameworkTags) (\s@CreateFramework' {} a -> s {frameworkTags = a} :: CreateFramework) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the framework. The name must be between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
createFramework_frameworkName :: Lens.Lens' CreateFramework Prelude.Text
createFramework_frameworkName = Lens.lens (\CreateFramework' {frameworkName} -> frameworkName) (\s@CreateFramework' {} a -> s {frameworkName = a} :: CreateFramework)

-- | A list of the controls that make up the framework. Each control in the
-- list has a name, input parameters, and scope.
createFramework_frameworkControls :: Lens.Lens' CreateFramework [FrameworkControl]
createFramework_frameworkControls = Lens.lens (\CreateFramework' {frameworkControls} -> frameworkControls) (\s@CreateFramework' {} a -> s {frameworkControls = a} :: CreateFramework) Prelude.. Lens.coerced

instance Core.AWSRequest CreateFramework where
  type
    AWSResponse CreateFramework =
      CreateFrameworkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFrameworkResponse'
            Prelude.<$> (x Data..?> "FrameworkArn")
            Prelude.<*> (x Data..?> "FrameworkName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFramework where
  hashWithSalt _salt CreateFramework' {..} =
    _salt `Prelude.hashWithSalt` frameworkDescription
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` frameworkTags
      `Prelude.hashWithSalt` frameworkName
      `Prelude.hashWithSalt` frameworkControls

instance Prelude.NFData CreateFramework where
  rnf CreateFramework' {..} =
    Prelude.rnf frameworkDescription
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf frameworkTags
      `Prelude.seq` Prelude.rnf frameworkName
      `Prelude.seq` Prelude.rnf frameworkControls

instance Data.ToHeaders CreateFramework where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFramework where
  toJSON CreateFramework' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FrameworkDescription" Data..=)
              Prelude.<$> frameworkDescription,
            ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("FrameworkTags" Data..=) Prelude.<$> frameworkTags,
            Prelude.Just ("FrameworkName" Data..= frameworkName),
            Prelude.Just
              ("FrameworkControls" Data..= frameworkControls)
          ]
      )

instance Data.ToPath CreateFramework where
  toPath = Prelude.const "/audit/frameworks"

instance Data.ToQuery CreateFramework where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFrameworkResponse' smart constructor.
data CreateFrameworkResponse = CreateFrameworkResponse'
  { -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    frameworkArn :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the framework. The name must be between 1 and 256
    -- characters, starting with a letter, and consisting of letters (a-z,
    -- A-Z), numbers (0-9), and underscores (_).
    frameworkName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFrameworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworkArn', 'createFrameworkResponse_frameworkArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'frameworkName', 'createFrameworkResponse_frameworkName' - The unique name of the framework. The name must be between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
--
-- 'httpStatus', 'createFrameworkResponse_httpStatus' - The response's http status code.
newCreateFrameworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFrameworkResponse
newCreateFrameworkResponse pHttpStatus_ =
  CreateFrameworkResponse'
    { frameworkArn =
        Prelude.Nothing,
      frameworkName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
createFrameworkResponse_frameworkArn :: Lens.Lens' CreateFrameworkResponse (Prelude.Maybe Prelude.Text)
createFrameworkResponse_frameworkArn = Lens.lens (\CreateFrameworkResponse' {frameworkArn} -> frameworkArn) (\s@CreateFrameworkResponse' {} a -> s {frameworkArn = a} :: CreateFrameworkResponse)

-- | The unique name of the framework. The name must be between 1 and 256
-- characters, starting with a letter, and consisting of letters (a-z,
-- A-Z), numbers (0-9), and underscores (_).
createFrameworkResponse_frameworkName :: Lens.Lens' CreateFrameworkResponse (Prelude.Maybe Prelude.Text)
createFrameworkResponse_frameworkName = Lens.lens (\CreateFrameworkResponse' {frameworkName} -> frameworkName) (\s@CreateFrameworkResponse' {} a -> s {frameworkName = a} :: CreateFrameworkResponse)

-- | The response's http status code.
createFrameworkResponse_httpStatus :: Lens.Lens' CreateFrameworkResponse Prelude.Int
createFrameworkResponse_httpStatus = Lens.lens (\CreateFrameworkResponse' {httpStatus} -> httpStatus) (\s@CreateFrameworkResponse' {} a -> s {httpStatus = a} :: CreateFrameworkResponse)

instance Prelude.NFData CreateFrameworkResponse where
  rnf CreateFrameworkResponse' {..} =
    Prelude.rnf frameworkArn
      `Prelude.seq` Prelude.rnf frameworkName
      `Prelude.seq` Prelude.rnf httpStatus
