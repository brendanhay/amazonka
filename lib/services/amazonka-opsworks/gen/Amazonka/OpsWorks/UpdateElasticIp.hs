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
-- Module      : Amazonka.OpsWorks.UpdateElasticIp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered Elastic IP address\'s name. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.UpdateElasticIp
  ( -- * Creating a Request
    UpdateElasticIp (..),
    newUpdateElasticIp,

    -- * Request Lenses
    updateElasticIp_name,
    updateElasticIp_elasticIp,

    -- * Destructuring the Response
    UpdateElasticIpResponse (..),
    newUpdateElasticIpResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateElasticIp' smart constructor.
data UpdateElasticIp = UpdateElasticIp'
  { -- | The new name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The IP address for which you want to update the name.
    elasticIp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateElasticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateElasticIp_name' - The new name.
--
-- 'elasticIp', 'updateElasticIp_elasticIp' - The IP address for which you want to update the name.
newUpdateElasticIp ::
  -- | 'elasticIp'
  Prelude.Text ->
  UpdateElasticIp
newUpdateElasticIp pElasticIp_ =
  UpdateElasticIp'
    { name = Prelude.Nothing,
      elasticIp = pElasticIp_
    }

-- | The new name.
updateElasticIp_name :: Lens.Lens' UpdateElasticIp (Prelude.Maybe Prelude.Text)
updateElasticIp_name = Lens.lens (\UpdateElasticIp' {name} -> name) (\s@UpdateElasticIp' {} a -> s {name = a} :: UpdateElasticIp)

-- | The IP address for which you want to update the name.
updateElasticIp_elasticIp :: Lens.Lens' UpdateElasticIp Prelude.Text
updateElasticIp_elasticIp = Lens.lens (\UpdateElasticIp' {elasticIp} -> elasticIp) (\s@UpdateElasticIp' {} a -> s {elasticIp = a} :: UpdateElasticIp)

instance Core.AWSRequest UpdateElasticIp where
  type
    AWSResponse UpdateElasticIp =
      UpdateElasticIpResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateElasticIpResponse'

instance Prelude.Hashable UpdateElasticIp where
  hashWithSalt _salt UpdateElasticIp' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` elasticIp

instance Prelude.NFData UpdateElasticIp where
  rnf UpdateElasticIp' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf elasticIp

instance Data.ToHeaders UpdateElasticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.UpdateElasticIp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateElasticIp where
  toJSON UpdateElasticIp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("ElasticIp" Data..= elasticIp)
          ]
      )

instance Data.ToPath UpdateElasticIp where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateElasticIp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateElasticIpResponse' smart constructor.
data UpdateElasticIpResponse = UpdateElasticIpResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateElasticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateElasticIpResponse ::
  UpdateElasticIpResponse
newUpdateElasticIpResponse = UpdateElasticIpResponse'

instance Prelude.NFData UpdateElasticIpResponse where
  rnf _ = ()
