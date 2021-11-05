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
-- Module      : Amazonka.RobOMaker.CreateRobot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a robot.
module Amazonka.RobOMaker.CreateRobot
  ( -- * Creating a Request
    CreateRobot (..),
    newCreateRobot,

    -- * Request Lenses
    createRobot_tags,
    createRobot_name,
    createRobot_architecture,
    createRobot_greengrassGroupId,

    -- * Destructuring the Response
    CreateRobotResponse (..),
    newCreateRobotResponse,

    -- * Response Lenses
    createRobotResponse_arn,
    createRobotResponse_createdAt,
    createRobotResponse_greengrassGroupId,
    createRobotResponse_name,
    createRobotResponse_architecture,
    createRobotResponse_tags,
    createRobotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateRobot' smart constructor.
data CreateRobot = CreateRobot'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- robot.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name for the robot.
    name :: Prelude.Text,
    -- | The target architecture of the robot.
    architecture :: Architecture,
    -- | The Greengrass group id.
    greengrassGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRobot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRobot_tags' - A map that contains tag keys and tag values that are attached to the
-- robot.
--
-- 'name', 'createRobot_name' - The name for the robot.
--
-- 'architecture', 'createRobot_architecture' - The target architecture of the robot.
--
-- 'greengrassGroupId', 'createRobot_greengrassGroupId' - The Greengrass group id.
newCreateRobot ::
  -- | 'name'
  Prelude.Text ->
  -- | 'architecture'
  Architecture ->
  -- | 'greengrassGroupId'
  Prelude.Text ->
  CreateRobot
newCreateRobot
  pName_
  pArchitecture_
  pGreengrassGroupId_ =
    CreateRobot'
      { tags = Prelude.Nothing,
        name = pName_,
        architecture = pArchitecture_,
        greengrassGroupId = pGreengrassGroupId_
      }

-- | A map that contains tag keys and tag values that are attached to the
-- robot.
createRobot_tags :: Lens.Lens' CreateRobot (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRobot_tags = Lens.lens (\CreateRobot' {tags} -> tags) (\s@CreateRobot' {} a -> s {tags = a} :: CreateRobot) Prelude.. Lens.mapping Lens.coerced

-- | The name for the robot.
createRobot_name :: Lens.Lens' CreateRobot Prelude.Text
createRobot_name = Lens.lens (\CreateRobot' {name} -> name) (\s@CreateRobot' {} a -> s {name = a} :: CreateRobot)

-- | The target architecture of the robot.
createRobot_architecture :: Lens.Lens' CreateRobot Architecture
createRobot_architecture = Lens.lens (\CreateRobot' {architecture} -> architecture) (\s@CreateRobot' {} a -> s {architecture = a} :: CreateRobot)

-- | The Greengrass group id.
createRobot_greengrassGroupId :: Lens.Lens' CreateRobot Prelude.Text
createRobot_greengrassGroupId = Lens.lens (\CreateRobot' {greengrassGroupId} -> greengrassGroupId) (\s@CreateRobot' {} a -> s {greengrassGroupId = a} :: CreateRobot)

instance Core.AWSRequest CreateRobot where
  type AWSResponse CreateRobot = CreateRobotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRobotResponse'
            Prelude.<$> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "greengrassGroupId")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "architecture")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRobot

instance Prelude.NFData CreateRobot

instance Core.ToHeaders CreateRobot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRobot where
  toJSON CreateRobot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("architecture" Core..= architecture),
            Prelude.Just
              ("greengrassGroupId" Core..= greengrassGroupId)
          ]
      )

instance Core.ToPath CreateRobot where
  toPath = Prelude.const "/createRobot"

instance Core.ToQuery CreateRobot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRobotResponse' smart constructor.
data CreateRobotResponse = CreateRobotResponse'
  { -- | The Amazon Resource Name (ARN) of the robot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the robot was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the Greengrass group associated with
    -- the robot.
    greengrassGroupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the robot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The target architecture of the robot.
    architecture :: Prelude.Maybe Architecture,
    -- | The list of all tags added to the robot.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRobotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createRobotResponse_arn' - The Amazon Resource Name (ARN) of the robot.
--
-- 'createdAt', 'createRobotResponse_createdAt' - The time, in milliseconds since the epoch, when the robot was created.
--
-- 'greengrassGroupId', 'createRobotResponse_greengrassGroupId' - The Amazon Resource Name (ARN) of the Greengrass group associated with
-- the robot.
--
-- 'name', 'createRobotResponse_name' - The name of the robot.
--
-- 'architecture', 'createRobotResponse_architecture' - The target architecture of the robot.
--
-- 'tags', 'createRobotResponse_tags' - The list of all tags added to the robot.
--
-- 'httpStatus', 'createRobotResponse_httpStatus' - The response's http status code.
newCreateRobotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRobotResponse
newCreateRobotResponse pHttpStatus_ =
  CreateRobotResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      greengrassGroupId = Prelude.Nothing,
      name = Prelude.Nothing,
      architecture = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the robot.
createRobotResponse_arn :: Lens.Lens' CreateRobotResponse (Prelude.Maybe Prelude.Text)
createRobotResponse_arn = Lens.lens (\CreateRobotResponse' {arn} -> arn) (\s@CreateRobotResponse' {} a -> s {arn = a} :: CreateRobotResponse)

-- | The time, in milliseconds since the epoch, when the robot was created.
createRobotResponse_createdAt :: Lens.Lens' CreateRobotResponse (Prelude.Maybe Prelude.UTCTime)
createRobotResponse_createdAt = Lens.lens (\CreateRobotResponse' {createdAt} -> createdAt) (\s@CreateRobotResponse' {} a -> s {createdAt = a} :: CreateRobotResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the Greengrass group associated with
-- the robot.
createRobotResponse_greengrassGroupId :: Lens.Lens' CreateRobotResponse (Prelude.Maybe Prelude.Text)
createRobotResponse_greengrassGroupId = Lens.lens (\CreateRobotResponse' {greengrassGroupId} -> greengrassGroupId) (\s@CreateRobotResponse' {} a -> s {greengrassGroupId = a} :: CreateRobotResponse)

-- | The name of the robot.
createRobotResponse_name :: Lens.Lens' CreateRobotResponse (Prelude.Maybe Prelude.Text)
createRobotResponse_name = Lens.lens (\CreateRobotResponse' {name} -> name) (\s@CreateRobotResponse' {} a -> s {name = a} :: CreateRobotResponse)

-- | The target architecture of the robot.
createRobotResponse_architecture :: Lens.Lens' CreateRobotResponse (Prelude.Maybe Architecture)
createRobotResponse_architecture = Lens.lens (\CreateRobotResponse' {architecture} -> architecture) (\s@CreateRobotResponse' {} a -> s {architecture = a} :: CreateRobotResponse)

-- | The list of all tags added to the robot.
createRobotResponse_tags :: Lens.Lens' CreateRobotResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRobotResponse_tags = Lens.lens (\CreateRobotResponse' {tags} -> tags) (\s@CreateRobotResponse' {} a -> s {tags = a} :: CreateRobotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createRobotResponse_httpStatus :: Lens.Lens' CreateRobotResponse Prelude.Int
createRobotResponse_httpStatus = Lens.lens (\CreateRobotResponse' {httpStatus} -> httpStatus) (\s@CreateRobotResponse' {} a -> s {httpStatus = a} :: CreateRobotResponse)

instance Prelude.NFData CreateRobotResponse
