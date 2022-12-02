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
-- Module      : Amazonka.GameLift.DescribeScript
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- __Related actions__
--
-- CreateScript | ListScripts | DescribeScript | UpdateScript |
-- DeleteScript |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DescribeScript
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeScript' smart constructor.
data DescribeScript = DescribeScript'
  { -- | A unique identifier for the Realtime script to retrieve properties for.
    -- You can use either the script ID or ARN value.
    scriptId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scriptId', 'describeScript_scriptId' - A unique identifier for the Realtime script to retrieve properties for.
-- You can use either the script ID or ARN value.
newDescribeScript ::
  -- | 'scriptId'
  Prelude.Text ->
  DescribeScript
newDescribeScript pScriptId_ =
  DescribeScript' {scriptId = pScriptId_}

-- | A unique identifier for the Realtime script to retrieve properties for.
-- You can use either the script ID or ARN value.
describeScript_scriptId :: Lens.Lens' DescribeScript Prelude.Text
describeScript_scriptId = Lens.lens (\DescribeScript' {scriptId} -> scriptId) (\s@DescribeScript' {} a -> s {scriptId = a} :: DescribeScript)

instance Core.AWSRequest DescribeScript where
  type
    AWSResponse DescribeScript =
      DescribeScriptResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScriptResponse'
            Prelude.<$> (x Data..?> "Script")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScript where
  hashWithSalt _salt DescribeScript' {..} =
    _salt `Prelude.hashWithSalt` scriptId

instance Prelude.NFData DescribeScript where
  rnf DescribeScript' {..} = Prelude.rnf scriptId

instance Data.ToHeaders DescribeScript where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DescribeScript" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeScript where
  toJSON DescribeScript' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ScriptId" Data..= scriptId)]
      )

instance Data.ToPath DescribeScript where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeScript where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeScriptResponse' smart constructor.
data DescribeScriptResponse = DescribeScriptResponse'
  { -- | A set of properties describing the requested script.
    script :: Prelude.Maybe Script,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeScriptResponse
newDescribeScriptResponse pHttpStatus_ =
  DescribeScriptResponse'
    { script = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A set of properties describing the requested script.
describeScriptResponse_script :: Lens.Lens' DescribeScriptResponse (Prelude.Maybe Script)
describeScriptResponse_script = Lens.lens (\DescribeScriptResponse' {script} -> script) (\s@DescribeScriptResponse' {} a -> s {script = a} :: DescribeScriptResponse)

-- | The response's http status code.
describeScriptResponse_httpStatus :: Lens.Lens' DescribeScriptResponse Prelude.Int
describeScriptResponse_httpStatus = Lens.lens (\DescribeScriptResponse' {httpStatus} -> httpStatus) (\s@DescribeScriptResponse' {} a -> s {httpStatus = a} :: DescribeScriptResponse)

instance Prelude.NFData DescribeScriptResponse where
  rnf DescribeScriptResponse' {..} =
    Prelude.rnf script
      `Prelude.seq` Prelude.rnf httpStatus
