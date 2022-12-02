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
-- Module      : Amazonka.PinpointEmail.DeleteConfigurationSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an existing configuration set.
--
-- In Amazon Pinpoint, /configuration sets/ are groups of rules that you
-- can apply to the emails you send. You apply a configuration set to an
-- email by including a reference to the configuration set in the headers
-- of the email. When you apply a configuration set to an email, all of the
-- rules in that configuration set are applied to the email.
module Amazonka.PinpointEmail.DeleteConfigurationSet
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to delete a configuration set.
--
-- /See:/ 'newDeleteConfigurationSet' smart constructor.
data DeleteConfigurationSet = DeleteConfigurationSet'
  { -- | The name of the configuration set that you want to delete.
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
-- 'configurationSetName', 'deleteConfigurationSet_configurationSetName' - The name of the configuration set that you want to delete.
newDeleteConfigurationSet ::
  -- | 'configurationSetName'
  Prelude.Text ->
  DeleteConfigurationSet
newDeleteConfigurationSet pConfigurationSetName_ =
  DeleteConfigurationSet'
    { configurationSetName =
        pConfigurationSetName_
    }

-- | The name of the configuration set that you want to delete.
deleteConfigurationSet_configurationSetName :: Lens.Lens' DeleteConfigurationSet Prelude.Text
deleteConfigurationSet_configurationSetName = Lens.lens (\DeleteConfigurationSet' {configurationSetName} -> configurationSetName) (\s@DeleteConfigurationSet' {} a -> s {configurationSetName = a} :: DeleteConfigurationSet)

instance Core.AWSRequest DeleteConfigurationSet where
  type
    AWSResponse DeleteConfigurationSet =
      DeleteConfigurationSetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
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

instance Data.ToHeaders DeleteConfigurationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteConfigurationSet where
  toPath DeleteConfigurationSet' {..} =
    Prelude.mconcat
      [ "/v1/email/configuration-sets/",
        Data.toBS configurationSetName
      ]

instance Data.ToQuery DeleteConfigurationSet where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
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
