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
-- Module      : Amazonka.Backup.DeleteFramework
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the framework specified by a framework name.
module Amazonka.Backup.DeleteFramework
  ( -- * Creating a Request
    DeleteFramework (..),
    newDeleteFramework,

    -- * Request Lenses
    deleteFramework_frameworkName,

    -- * Destructuring the Response
    DeleteFrameworkResponse (..),
    newDeleteFrameworkResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFramework' smart constructor.
data DeleteFramework = DeleteFramework'
  { -- | The unique name of a framework.
    frameworkName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFramework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworkName', 'deleteFramework_frameworkName' - The unique name of a framework.
newDeleteFramework ::
  -- | 'frameworkName'
  Prelude.Text ->
  DeleteFramework
newDeleteFramework pFrameworkName_ =
  DeleteFramework' {frameworkName = pFrameworkName_}

-- | The unique name of a framework.
deleteFramework_frameworkName :: Lens.Lens' DeleteFramework Prelude.Text
deleteFramework_frameworkName = Lens.lens (\DeleteFramework' {frameworkName} -> frameworkName) (\s@DeleteFramework' {} a -> s {frameworkName = a} :: DeleteFramework)

instance Core.AWSRequest DeleteFramework where
  type
    AWSResponse DeleteFramework =
      DeleteFrameworkResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteFrameworkResponse'

instance Prelude.Hashable DeleteFramework where
  hashWithSalt _salt DeleteFramework' {..} =
    _salt `Prelude.hashWithSalt` frameworkName

instance Prelude.NFData DeleteFramework where
  rnf DeleteFramework' {..} = Prelude.rnf frameworkName

instance Core.ToHeaders DeleteFramework where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteFramework where
  toPath DeleteFramework' {..} =
    Prelude.mconcat
      ["/audit/frameworks/", Core.toBS frameworkName]

instance Core.ToQuery DeleteFramework where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFrameworkResponse' smart constructor.
data DeleteFrameworkResponse = DeleteFrameworkResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFrameworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFrameworkResponse ::
  DeleteFrameworkResponse
newDeleteFrameworkResponse = DeleteFrameworkResponse'

instance Prelude.NFData DeleteFrameworkResponse where
  rnf _ = ()
