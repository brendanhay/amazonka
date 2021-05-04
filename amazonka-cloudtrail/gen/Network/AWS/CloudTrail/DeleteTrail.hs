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
-- Module      : Network.AWS.CloudTrail.DeleteTrail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a trail. This operation must be called from the region in which
-- the trail was created. @DeleteTrail@ cannot be called on the shadow
-- trails (replicated trails in other regions) of a trail that is enabled
-- in all regions.
module Network.AWS.CloudTrail.DeleteTrail
  ( -- * Creating a Request
    DeleteTrail (..),
    newDeleteTrail,

    -- * Request Lenses
    deleteTrail_name,

    -- * Destructuring the Response
    DeleteTrailResponse (..),
    newDeleteTrailResponse,

    -- * Response Lenses
    deleteTrailResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request that specifies the name of a trail to delete.
--
-- /See:/ 'newDeleteTrail' smart constructor.
data DeleteTrail = DeleteTrail'
  { -- | Specifies the name or the CloudTrail ARN of the trail to be deleted. The
    -- format of a trail ARN is:
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteTrail_name' - Specifies the name or the CloudTrail ARN of the trail to be deleted. The
-- format of a trail ARN is:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newDeleteTrail ::
  -- | 'name'
  Prelude.Text ->
  DeleteTrail
newDeleteTrail pName_ = DeleteTrail' {name = pName_}

-- | Specifies the name or the CloudTrail ARN of the trail to be deleted. The
-- format of a trail ARN is:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
deleteTrail_name :: Lens.Lens' DeleteTrail Prelude.Text
deleteTrail_name = Lens.lens (\DeleteTrail' {name} -> name) (\s@DeleteTrail' {} a -> s {name = a} :: DeleteTrail)

instance Prelude.AWSRequest DeleteTrail where
  type Rs DeleteTrail = DeleteTrailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTrailResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrail

instance Prelude.NFData DeleteTrail

instance Prelude.ToHeaders DeleteTrail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DeleteTrail" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTrail where
  toJSON DeleteTrail' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteTrail where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTrail where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newDeleteTrailResponse' smart constructor.
data DeleteTrailResponse = DeleteTrailResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTrailResponse_httpStatus' - The response's http status code.
newDeleteTrailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrailResponse
newDeleteTrailResponse pHttpStatus_ =
  DeleteTrailResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTrailResponse_httpStatus :: Lens.Lens' DeleteTrailResponse Prelude.Int
deleteTrailResponse_httpStatus = Lens.lens (\DeleteTrailResponse' {httpStatus} -> httpStatus) (\s@DeleteTrailResponse' {} a -> s {httpStatus = a} :: DeleteTrailResponse)

instance Prelude.NFData DeleteTrailResponse
