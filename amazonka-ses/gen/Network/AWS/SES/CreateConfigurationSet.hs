{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.CreateConfigurationSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.SES.CreateConfigurationSet
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CreateConfigurationSet where
  type
    Rs CreateConfigurationSet =
      CreateConfigurationSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateConfigurationSetResult"
      ( \s h x ->
          CreateConfigurationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConfigurationSet

instance Prelude.NFData CreateConfigurationSet

instance Prelude.ToHeaders CreateConfigurationSet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateConfigurationSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateConfigurationSet where
  toQuery CreateConfigurationSet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateConfigurationSet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSet" Prelude.=: configurationSet
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCreateConfigurationSetResponse' smart constructor.
data CreateConfigurationSetResponse = CreateConfigurationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
