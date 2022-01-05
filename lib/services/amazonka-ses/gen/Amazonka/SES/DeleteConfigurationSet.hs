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
-- Module      : Amazonka.SES.DeleteConfigurationSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration set. Configuration sets enable you to publish
-- email sending events. For information about using configuration sets,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.DeleteConfigurationSet
  ( -- * Creating a Request
    DeleteConfigurationSet (..),
    newDeleteConfigurationSet,

    -- * Request Lenses
    deleteConfigurationSet_configurationSetName,

    -- * Destructuring the Response
    DeleteConfigurationSetResponse (..),
    newDeleteConfigurationSetResponse,

    -- * Response Lenses
    deleteConfigurationSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to delete a configuration set. Configuration sets
-- enable you to publish email sending events. For information about using
-- configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteConfigurationSet' smart constructor.
data DeleteConfigurationSet = DeleteConfigurationSet'
  { -- | The name of the configuration set to delete.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'deleteConfigurationSet_configurationSetName' - The name of the configuration set to delete.
newDeleteConfigurationSet ::
  -- | 'configurationSetName'
  Prelude.Text ->
  DeleteConfigurationSet
newDeleteConfigurationSet pConfigurationSetName_ =
  DeleteConfigurationSet'
    { configurationSetName =
        pConfigurationSetName_
    }

-- | The name of the configuration set to delete.
deleteConfigurationSet_configurationSetName :: Lens.Lens' DeleteConfigurationSet Prelude.Text
deleteConfigurationSet_configurationSetName = Lens.lens (\DeleteConfigurationSet' {configurationSetName} -> configurationSetName) (\s@DeleteConfigurationSet' {} a -> s {configurationSetName = a} :: DeleteConfigurationSet)

instance Core.AWSRequest DeleteConfigurationSet where
  type
    AWSResponse DeleteConfigurationSet =
      DeleteConfigurationSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteConfigurationSetResult"
      ( \s h x ->
          DeleteConfigurationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConfigurationSet where
  hashWithSalt _salt DeleteConfigurationSet' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName

instance Prelude.NFData DeleteConfigurationSet where
  rnf DeleteConfigurationSet' {..} =
    Prelude.rnf configurationSetName

instance Core.ToHeaders DeleteConfigurationSet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteConfigurationSet where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteConfigurationSet where
  toQuery DeleteConfigurationSet' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteConfigurationSet" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName" Core.=: configurationSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteConfigurationSetResponse' smart constructor.
data DeleteConfigurationSetResponse = DeleteConfigurationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConfigurationSetResponse_httpStatus' - The response's http status code.
newDeleteConfigurationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConfigurationSetResponse
newDeleteConfigurationSetResponse pHttpStatus_ =
  DeleteConfigurationSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConfigurationSetResponse_httpStatus :: Lens.Lens' DeleteConfigurationSetResponse Prelude.Int
deleteConfigurationSetResponse_httpStatus = Lens.lens (\DeleteConfigurationSetResponse' {httpStatus} -> httpStatus) (\s@DeleteConfigurationSetResponse' {} a -> s {httpStatus = a} :: DeleteConfigurationSetResponse)

instance
  Prelude.NFData
    DeleteConfigurationSetResponse
  where
  rnf DeleteConfigurationSetResponse' {..} =
    Prelude.rnf httpStatus
