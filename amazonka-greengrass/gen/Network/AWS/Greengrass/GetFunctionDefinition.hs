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
-- Module      : Network.AWS.Greengrass.GetFunctionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Lambda function definition, including its
-- creation time and latest version.
module Network.AWS.Greengrass.GetFunctionDefinition
  ( -- * Creating a Request
    GetFunctionDefinition (..),
    newGetFunctionDefinition,

    -- * Request Lenses
    getFunctionDefinition_functionDefinitionId,

    -- * Destructuring the Response
    GetFunctionDefinitionResponse (..),
    newGetFunctionDefinitionResponse,

    -- * Response Lenses
    getFunctionDefinitionResponse_creationTimestamp,
    getFunctionDefinitionResponse_latestVersionArn,
    getFunctionDefinitionResponse_latestVersion,
    getFunctionDefinitionResponse_arn,
    getFunctionDefinitionResponse_id,
    getFunctionDefinitionResponse_name,
    getFunctionDefinitionResponse_lastUpdatedTimestamp,
    getFunctionDefinitionResponse_tags,
    getFunctionDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFunctionDefinition' smart constructor.
data GetFunctionDefinition = GetFunctionDefinition'
  { -- | The ID of the Lambda function definition.
    functionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFunctionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionDefinitionId', 'getFunctionDefinition_functionDefinitionId' - The ID of the Lambda function definition.
newGetFunctionDefinition ::
  -- | 'functionDefinitionId'
  Prelude.Text ->
  GetFunctionDefinition
newGetFunctionDefinition pFunctionDefinitionId_ =
  GetFunctionDefinition'
    { functionDefinitionId =
        pFunctionDefinitionId_
    }

-- | The ID of the Lambda function definition.
getFunctionDefinition_functionDefinitionId :: Lens.Lens' GetFunctionDefinition Prelude.Text
getFunctionDefinition_functionDefinitionId = Lens.lens (\GetFunctionDefinition' {functionDefinitionId} -> functionDefinitionId) (\s@GetFunctionDefinition' {} a -> s {functionDefinitionId = a} :: GetFunctionDefinition)

instance Core.AWSRequest GetFunctionDefinition where
  type
    AWSResponse GetFunctionDefinition =
      GetFunctionDefinitionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionDefinitionResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "LatestVersionArn")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFunctionDefinition

instance Prelude.NFData GetFunctionDefinition

instance Core.ToHeaders GetFunctionDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetFunctionDefinition where
  toPath GetFunctionDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/functions/",
        Core.toBS functionDefinitionId
      ]

instance Core.ToQuery GetFunctionDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFunctionDefinitionResponse' smart constructor.
data GetFunctionDefinitionResponse = GetFunctionDefinitionResponse'
  { -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFunctionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getFunctionDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getFunctionDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getFunctionDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getFunctionDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getFunctionDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getFunctionDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getFunctionDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getFunctionDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getFunctionDefinitionResponse_httpStatus' - The response's http status code.
newGetFunctionDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFunctionDefinitionResponse
newGetFunctionDefinitionResponse pHttpStatus_ =
  GetFunctionDefinitionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getFunctionDefinitionResponse_creationTimestamp :: Lens.Lens' GetFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionResponse_creationTimestamp = Lens.lens (\GetFunctionDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetFunctionDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetFunctionDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getFunctionDefinitionResponse_latestVersionArn :: Lens.Lens' GetFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionResponse_latestVersionArn = Lens.lens (\GetFunctionDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetFunctionDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetFunctionDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getFunctionDefinitionResponse_latestVersion :: Lens.Lens' GetFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionResponse_latestVersion = Lens.lens (\GetFunctionDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetFunctionDefinitionResponse' {} a -> s {latestVersion = a} :: GetFunctionDefinitionResponse)

-- | The ARN of the definition.
getFunctionDefinitionResponse_arn :: Lens.Lens' GetFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionResponse_arn = Lens.lens (\GetFunctionDefinitionResponse' {arn} -> arn) (\s@GetFunctionDefinitionResponse' {} a -> s {arn = a} :: GetFunctionDefinitionResponse)

-- | The ID of the definition.
getFunctionDefinitionResponse_id :: Lens.Lens' GetFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionResponse_id = Lens.lens (\GetFunctionDefinitionResponse' {id} -> id) (\s@GetFunctionDefinitionResponse' {} a -> s {id = a} :: GetFunctionDefinitionResponse)

-- | The name of the definition.
getFunctionDefinitionResponse_name :: Lens.Lens' GetFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionResponse_name = Lens.lens (\GetFunctionDefinitionResponse' {name} -> name) (\s@GetFunctionDefinitionResponse' {} a -> s {name = a} :: GetFunctionDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getFunctionDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetFunctionDefinitionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetFunctionDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetFunctionDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetFunctionDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getFunctionDefinitionResponse_tags :: Lens.Lens' GetFunctionDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getFunctionDefinitionResponse_tags = Lens.lens (\GetFunctionDefinitionResponse' {tags} -> tags) (\s@GetFunctionDefinitionResponse' {} a -> s {tags = a} :: GetFunctionDefinitionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getFunctionDefinitionResponse_httpStatus :: Lens.Lens' GetFunctionDefinitionResponse Prelude.Int
getFunctionDefinitionResponse_httpStatus = Lens.lens (\GetFunctionDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetFunctionDefinitionResponse' {} a -> s {httpStatus = a} :: GetFunctionDefinitionResponse)

instance Prelude.NFData GetFunctionDefinitionResponse
