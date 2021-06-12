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
-- Module      : Network.AWS.GameLift.DescribeScript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for a Realtime script.
--
-- To request a script record, specify the script ID. If successful, an
-- object containing the script properties is returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
--
-- __Related operations__
--
-- -   CreateScript
--
-- -   ListScripts
--
-- -   DescribeScript
--
-- -   UpdateScript
--
-- -   DeleteScript
module Network.AWS.GameLift.DescribeScript
  ( -- * Creating a Request
    DescribeScript (..),
    newDescribeScript,

    -- * Request Lenses
    describeScript_scriptId,

    -- * Destructuring the Response
    DescribeScriptResponse (..),
    newDescribeScriptResponse,

    -- * Response Lenses
    describeScriptResponse_script,
    describeScriptResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeScript' smart constructor.
data DescribeScript = DescribeScript'
  { -- | A unique identifier for a Realtime script to retrieve properties for.
    -- You can use either the script ID or ARN value.
    scriptId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scriptId', 'describeScript_scriptId' - A unique identifier for a Realtime script to retrieve properties for.
-- You can use either the script ID or ARN value.
newDescribeScript ::
  -- | 'scriptId'
  Core.Text ->
  DescribeScript
newDescribeScript pScriptId_ =
  DescribeScript' {scriptId = pScriptId_}

-- | A unique identifier for a Realtime script to retrieve properties for.
-- You can use either the script ID or ARN value.
describeScript_scriptId :: Lens.Lens' DescribeScript Core.Text
describeScript_scriptId = Lens.lens (\DescribeScript' {scriptId} -> scriptId) (\s@DescribeScript' {} a -> s {scriptId = a} :: DescribeScript)

instance Core.AWSRequest DescribeScript where
  type
    AWSResponse DescribeScript =
      DescribeScriptResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScriptResponse'
            Core.<$> (x Core..?> "Script")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeScript

instance Core.NFData DescribeScript

instance Core.ToHeaders DescribeScript where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.DescribeScript" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeScript where
  toJSON DescribeScript' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ScriptId" Core..= scriptId)]
      )

instance Core.ToPath DescribeScript where
  toPath = Core.const "/"

instance Core.ToQuery DescribeScript where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeScriptResponse' smart constructor.
data DescribeScriptResponse = DescribeScriptResponse'
  { -- | A set of properties describing the requested script.
    script :: Core.Maybe Script,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeScriptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'script', 'describeScriptResponse_script' - A set of properties describing the requested script.
--
-- 'httpStatus', 'describeScriptResponse_httpStatus' - The response's http status code.
newDescribeScriptResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeScriptResponse
newDescribeScriptResponse pHttpStatus_ =
  DescribeScriptResponse'
    { script = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A set of properties describing the requested script.
describeScriptResponse_script :: Lens.Lens' DescribeScriptResponse (Core.Maybe Script)
describeScriptResponse_script = Lens.lens (\DescribeScriptResponse' {script} -> script) (\s@DescribeScriptResponse' {} a -> s {script = a} :: DescribeScriptResponse)

-- | The response's http status code.
describeScriptResponse_httpStatus :: Lens.Lens' DescribeScriptResponse Core.Int
describeScriptResponse_httpStatus = Lens.lens (\DescribeScriptResponse' {httpStatus} -> httpStatus) (\s@DescribeScriptResponse' {} a -> s {httpStatus = a} :: DescribeScriptResponse)

instance Core.NFData DescribeScriptResponse
