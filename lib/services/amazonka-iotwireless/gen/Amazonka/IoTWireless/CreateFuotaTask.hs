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
-- Module      : Amazonka.IoTWireless.CreateFuotaTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a FUOTA task.
module Amazonka.IoTWireless.CreateFuotaTask
  ( -- * Creating a Request
    CreateFuotaTask (..),
    newCreateFuotaTask,

    -- * Request Lenses
    createFuotaTask_clientRequestToken,
    createFuotaTask_description,
    createFuotaTask_fragmentIntervalMS,
    createFuotaTask_fragmentSizeBytes,
    createFuotaTask_loRaWAN,
    createFuotaTask_name,
    createFuotaTask_redundancyPercent,
    createFuotaTask_tags,
    createFuotaTask_firmwareUpdateImage,
    createFuotaTask_firmwareUpdateRole,

    -- * Destructuring the Response
    CreateFuotaTaskResponse (..),
    newCreateFuotaTaskResponse,

    -- * Response Lenses
    createFuotaTaskResponse_arn,
    createFuotaTaskResponse_id,
    createFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFuotaTask' smart constructor.
data CreateFuotaTask = CreateFuotaTask'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    fragmentIntervalMS :: Prelude.Maybe Prelude.Natural,
    fragmentSizeBytes :: Prelude.Maybe Prelude.Natural,
    loRaWAN :: Prelude.Maybe LoRaWANFuotaTask,
    name :: Prelude.Maybe Prelude.Text,
    redundancyPercent :: Prelude.Maybe Prelude.Natural,
    tags :: Prelude.Maybe [Tag],
    firmwareUpdateImage :: Prelude.Text,
    firmwareUpdateRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createFuotaTask_clientRequestToken' - Undocumented member.
--
-- 'description', 'createFuotaTask_description' - Undocumented member.
--
-- 'fragmentIntervalMS', 'createFuotaTask_fragmentIntervalMS' - Undocumented member.
--
-- 'fragmentSizeBytes', 'createFuotaTask_fragmentSizeBytes' - Undocumented member.
--
-- 'loRaWAN', 'createFuotaTask_loRaWAN' - Undocumented member.
--
-- 'name', 'createFuotaTask_name' - Undocumented member.
--
-- 'redundancyPercent', 'createFuotaTask_redundancyPercent' - Undocumented member.
--
-- 'tags', 'createFuotaTask_tags' - Undocumented member.
--
-- 'firmwareUpdateImage', 'createFuotaTask_firmwareUpdateImage' - Undocumented member.
--
-- 'firmwareUpdateRole', 'createFuotaTask_firmwareUpdateRole' - Undocumented member.
newCreateFuotaTask ::
  -- | 'firmwareUpdateImage'
  Prelude.Text ->
  -- | 'firmwareUpdateRole'
  Prelude.Text ->
  CreateFuotaTask
newCreateFuotaTask
  pFirmwareUpdateImage_
  pFirmwareUpdateRole_ =
    CreateFuotaTask'
      { clientRequestToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        fragmentIntervalMS = Prelude.Nothing,
        fragmentSizeBytes = Prelude.Nothing,
        loRaWAN = Prelude.Nothing,
        name = Prelude.Nothing,
        redundancyPercent = Prelude.Nothing,
        tags = Prelude.Nothing,
        firmwareUpdateImage = pFirmwareUpdateImage_,
        firmwareUpdateRole = pFirmwareUpdateRole_
      }

-- | Undocumented member.
createFuotaTask_clientRequestToken :: Lens.Lens' CreateFuotaTask (Prelude.Maybe Prelude.Text)
createFuotaTask_clientRequestToken = Lens.lens (\CreateFuotaTask' {clientRequestToken} -> clientRequestToken) (\s@CreateFuotaTask' {} a -> s {clientRequestToken = a} :: CreateFuotaTask)

-- | Undocumented member.
createFuotaTask_description :: Lens.Lens' CreateFuotaTask (Prelude.Maybe Prelude.Text)
createFuotaTask_description = Lens.lens (\CreateFuotaTask' {description} -> description) (\s@CreateFuotaTask' {} a -> s {description = a} :: CreateFuotaTask)

-- | Undocumented member.
createFuotaTask_fragmentIntervalMS :: Lens.Lens' CreateFuotaTask (Prelude.Maybe Prelude.Natural)
createFuotaTask_fragmentIntervalMS = Lens.lens (\CreateFuotaTask' {fragmentIntervalMS} -> fragmentIntervalMS) (\s@CreateFuotaTask' {} a -> s {fragmentIntervalMS = a} :: CreateFuotaTask)

-- | Undocumented member.
createFuotaTask_fragmentSizeBytes :: Lens.Lens' CreateFuotaTask (Prelude.Maybe Prelude.Natural)
createFuotaTask_fragmentSizeBytes = Lens.lens (\CreateFuotaTask' {fragmentSizeBytes} -> fragmentSizeBytes) (\s@CreateFuotaTask' {} a -> s {fragmentSizeBytes = a} :: CreateFuotaTask)

-- | Undocumented member.
createFuotaTask_loRaWAN :: Lens.Lens' CreateFuotaTask (Prelude.Maybe LoRaWANFuotaTask)
createFuotaTask_loRaWAN = Lens.lens (\CreateFuotaTask' {loRaWAN} -> loRaWAN) (\s@CreateFuotaTask' {} a -> s {loRaWAN = a} :: CreateFuotaTask)

-- | Undocumented member.
createFuotaTask_name :: Lens.Lens' CreateFuotaTask (Prelude.Maybe Prelude.Text)
createFuotaTask_name = Lens.lens (\CreateFuotaTask' {name} -> name) (\s@CreateFuotaTask' {} a -> s {name = a} :: CreateFuotaTask)

-- | Undocumented member.
createFuotaTask_redundancyPercent :: Lens.Lens' CreateFuotaTask (Prelude.Maybe Prelude.Natural)
createFuotaTask_redundancyPercent = Lens.lens (\CreateFuotaTask' {redundancyPercent} -> redundancyPercent) (\s@CreateFuotaTask' {} a -> s {redundancyPercent = a} :: CreateFuotaTask)

-- | Undocumented member.
createFuotaTask_tags :: Lens.Lens' CreateFuotaTask (Prelude.Maybe [Tag])
createFuotaTask_tags = Lens.lens (\CreateFuotaTask' {tags} -> tags) (\s@CreateFuotaTask' {} a -> s {tags = a} :: CreateFuotaTask) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createFuotaTask_firmwareUpdateImage :: Lens.Lens' CreateFuotaTask Prelude.Text
createFuotaTask_firmwareUpdateImage = Lens.lens (\CreateFuotaTask' {firmwareUpdateImage} -> firmwareUpdateImage) (\s@CreateFuotaTask' {} a -> s {firmwareUpdateImage = a} :: CreateFuotaTask)

-- | Undocumented member.
createFuotaTask_firmwareUpdateRole :: Lens.Lens' CreateFuotaTask Prelude.Text
createFuotaTask_firmwareUpdateRole = Lens.lens (\CreateFuotaTask' {firmwareUpdateRole} -> firmwareUpdateRole) (\s@CreateFuotaTask' {} a -> s {firmwareUpdateRole = a} :: CreateFuotaTask)

instance Core.AWSRequest CreateFuotaTask where
  type
    AWSResponse CreateFuotaTask =
      CreateFuotaTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFuotaTaskResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFuotaTask where
  hashWithSalt _salt CreateFuotaTask' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fragmentIntervalMS
      `Prelude.hashWithSalt` fragmentSizeBytes
      `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` redundancyPercent
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` firmwareUpdateImage
      `Prelude.hashWithSalt` firmwareUpdateRole

instance Prelude.NFData CreateFuotaTask where
  rnf CreateFuotaTask' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf fragmentIntervalMS
      `Prelude.seq` Prelude.rnf fragmentSizeBytes
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf redundancyPercent
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf firmwareUpdateImage
      `Prelude.seq` Prelude.rnf firmwareUpdateRole

instance Data.ToHeaders CreateFuotaTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateFuotaTask where
  toJSON CreateFuotaTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("FragmentIntervalMS" Data..=)
              Prelude.<$> fragmentIntervalMS,
            ("FragmentSizeBytes" Data..=)
              Prelude.<$> fragmentSizeBytes,
            ("LoRaWAN" Data..=) Prelude.<$> loRaWAN,
            ("Name" Data..=) Prelude.<$> name,
            ("RedundancyPercent" Data..=)
              Prelude.<$> redundancyPercent,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("FirmwareUpdateImage" Data..= firmwareUpdateImage),
            Prelude.Just
              ("FirmwareUpdateRole" Data..= firmwareUpdateRole)
          ]
      )

instance Data.ToPath CreateFuotaTask where
  toPath = Prelude.const "/fuota-tasks"

instance Data.ToQuery CreateFuotaTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFuotaTaskResponse' smart constructor.
data CreateFuotaTaskResponse = CreateFuotaTaskResponse'
  { arn :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createFuotaTaskResponse_arn' - Undocumented member.
--
-- 'id', 'createFuotaTaskResponse_id' - Undocumented member.
--
-- 'httpStatus', 'createFuotaTaskResponse_httpStatus' - The response's http status code.
newCreateFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFuotaTaskResponse
newCreateFuotaTaskResponse pHttpStatus_ =
  CreateFuotaTaskResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createFuotaTaskResponse_arn :: Lens.Lens' CreateFuotaTaskResponse (Prelude.Maybe Prelude.Text)
createFuotaTaskResponse_arn = Lens.lens (\CreateFuotaTaskResponse' {arn} -> arn) (\s@CreateFuotaTaskResponse' {} a -> s {arn = a} :: CreateFuotaTaskResponse)

-- | Undocumented member.
createFuotaTaskResponse_id :: Lens.Lens' CreateFuotaTaskResponse (Prelude.Maybe Prelude.Text)
createFuotaTaskResponse_id = Lens.lens (\CreateFuotaTaskResponse' {id} -> id) (\s@CreateFuotaTaskResponse' {} a -> s {id = a} :: CreateFuotaTaskResponse)

-- | The response's http status code.
createFuotaTaskResponse_httpStatus :: Lens.Lens' CreateFuotaTaskResponse Prelude.Int
createFuotaTaskResponse_httpStatus = Lens.lens (\CreateFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@CreateFuotaTaskResponse' {} a -> s {httpStatus = a} :: CreateFuotaTaskResponse)

instance Prelude.NFData CreateFuotaTaskResponse where
  rnf CreateFuotaTaskResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
