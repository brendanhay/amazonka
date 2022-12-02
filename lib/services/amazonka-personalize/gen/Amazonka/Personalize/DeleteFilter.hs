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
-- Module      : Amazonka.Personalize.DeleteFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a filter.
module Amazonka.Personalize.DeleteFilter
  ( -- * Creating a Request
    DeleteFilter (..),
    newDeleteFilter,

    -- * Request Lenses
    deleteFilter_filterArn,

    -- * Destructuring the Response
    DeleteFilterResponse (..),
    newDeleteFilterResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFilter' smart constructor.
data DeleteFilter = DeleteFilter'
  { -- | The ARN of the filter to delete.
    filterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterArn', 'deleteFilter_filterArn' - The ARN of the filter to delete.
newDeleteFilter ::
  -- | 'filterArn'
  Prelude.Text ->
  DeleteFilter
newDeleteFilter pFilterArn_ =
  DeleteFilter' {filterArn = pFilterArn_}

-- | The ARN of the filter to delete.
deleteFilter_filterArn :: Lens.Lens' DeleteFilter Prelude.Text
deleteFilter_filterArn = Lens.lens (\DeleteFilter' {filterArn} -> filterArn) (\s@DeleteFilter' {} a -> s {filterArn = a} :: DeleteFilter)

instance Core.AWSRequest DeleteFilter where
  type AWSResponse DeleteFilter = DeleteFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteFilterResponse'

instance Prelude.Hashable DeleteFilter where
  hashWithSalt _salt DeleteFilter' {..} =
    _salt `Prelude.hashWithSalt` filterArn

instance Prelude.NFData DeleteFilter where
  rnf DeleteFilter' {..} = Prelude.rnf filterArn

instance Data.ToHeaders DeleteFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DeleteFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFilter where
  toJSON DeleteFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("filterArn" Data..= filterArn)]
      )

instance Data.ToPath DeleteFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFilterResponse' smart constructor.
data DeleteFilterResponse = DeleteFilterResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFilterResponse ::
  DeleteFilterResponse
newDeleteFilterResponse = DeleteFilterResponse'

instance Prelude.NFData DeleteFilterResponse where
  rnf _ = ()
