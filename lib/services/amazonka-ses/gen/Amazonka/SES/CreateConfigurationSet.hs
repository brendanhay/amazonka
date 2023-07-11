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
-- Module      : Amazonka.SES.CreateConfigurationSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration set.
--
-- Configuration sets enable you to publish email sending events. For
-- information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.CreateConfigurationSet
  ( -- * Creating a Request
    CreateConfigurationSet (..),
    newCreateConfigurationSet,

    -- * Request Lenses
    createConfigurationSet_configurationSet,

    -- * Destructuring the Response
    CreateConfigurationSetResponse (..),
    newCreateConfigurationSetResponse,

    -- * Response Lenses
    createConfigurationSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to create a configuration set. Configuration sets
-- enable you to publish email sending events. For information about using
-- configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCreateConfigurationSet' smart constructor.
data CreateConfigurationSet = CreateConfigurationSet'
  { -- | A data structure that contains the name of the configuration set.
    configurationSet :: ConfigurationSet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSet', 'createConfigurationSet_configurationSet' - A data structure that contains the name of the configuration set.
newCreateConfigurationSet ::
  -- | 'configurationSet'
  ConfigurationSet ->
  CreateConfigurationSet
newCreateConfigurationSet pConfigurationSet_ =
  CreateConfigurationSet'
    { configurationSet =
        pConfigurationSet_
    }

-- | A data structure that contains the name of the configuration set.
createConfigurationSet_configurationSet :: Lens.Lens' CreateConfigurationSet ConfigurationSet
createConfigurationSet_configurationSet = Lens.lens (\CreateConfigurationSet' {configurationSet} -> configurationSet) (\s@CreateConfigurationSet' {} a -> s {configurationSet = a} :: CreateConfigurationSet)

instance Core.AWSRequest CreateConfigurationSet where
  type
    AWSResponse CreateConfigurationSet =
      CreateConfigurationSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateConfigurationSetResult"
      ( \s h x ->
          CreateConfigurationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConfigurationSet where
  hashWithSalt _salt CreateConfigurationSet' {..} =
    _salt `Prelude.hashWithSalt` configurationSet

instance Prelude.NFData CreateConfigurationSet where
  rnf CreateConfigurationSet' {..} =
    Prelude.rnf configurationSet

instance Data.ToHeaders CreateConfigurationSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateConfigurationSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConfigurationSet where
  toQuery CreateConfigurationSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateConfigurationSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSet" Data.=: configurationSet
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCreateConfigurationSetResponse' smart constructor.
data CreateConfigurationSetResponse = CreateConfigurationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConfigurationSetResponse_httpStatus' - The response's http status code.
newCreateConfigurationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConfigurationSetResponse
newCreateConfigurationSetResponse pHttpStatus_ =
  CreateConfigurationSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createConfigurationSetResponse_httpStatus :: Lens.Lens' CreateConfigurationSetResponse Prelude.Int
createConfigurationSetResponse_httpStatus = Lens.lens (\CreateConfigurationSetResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationSetResponse' {} a -> s {httpStatus = a} :: CreateConfigurationSetResponse)

instance
  Prelude.NFData
    CreateConfigurationSetResponse
  where
  rnf CreateConfigurationSetResponse' {..} =
    Prelude.rnf httpStatus
