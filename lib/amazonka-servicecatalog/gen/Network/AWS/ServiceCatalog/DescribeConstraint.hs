{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified constraint.
module Network.AWS.ServiceCatalog.DescribeConstraint
  ( -- * Creating a request
    DescribeConstraint (..),
    mkDescribeConstraint,

    -- ** Request lenses
    dcAcceptLanguage,
    dcId,

    -- * Destructuring the response
    DescribeConstraintResponse (..),
    mkDescribeConstraintResponse,

    -- ** Response lenses
    dcrsStatus,
    dcrsConstraintDetail,
    dcrsConstraintParameters,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeConstraint' smart constructor.
data DescribeConstraint = DescribeConstraint'
  { -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | The identifier of the constraint.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConstraint' with the minimum fields required to make a request.
--
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'id' - The identifier of the constraint.
mkDescribeConstraint ::
  -- | 'id'
  Lude.Text ->
  DescribeConstraint
mkDescribeConstraint pId_ =
  DescribeConstraint' {acceptLanguage = Lude.Nothing, id = pId_}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAcceptLanguage :: Lens.Lens' DescribeConstraint (Lude.Maybe Lude.Text)
dcAcceptLanguage = Lens.lens (acceptLanguage :: DescribeConstraint -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeConstraint)
{-# DEPRECATED dcAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The identifier of the constraint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcId :: Lens.Lens' DescribeConstraint Lude.Text
dcId = Lens.lens (id :: DescribeConstraint -> Lude.Text) (\s a -> s {id = a} :: DescribeConstraint)
{-# DEPRECATED dcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeConstraint where
  type Rs DescribeConstraint = DescribeConstraintResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConstraintResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "ConstraintDetail")
            Lude.<*> (x Lude..?> "ConstraintParameters")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConstraint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeConstraint" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConstraint where
  toJSON DescribeConstraint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath DescribeConstraint where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConstraint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConstraintResponse' smart constructor.
data DescribeConstraintResponse = DescribeConstraintResponse'
  { -- | The status of the current request.
    status :: Lude.Maybe RequestStatus,
    -- | Information about the constraint.
    constraintDetail :: Lude.Maybe ConstraintDetail,
    -- | The constraint parameters.
    constraintParameters :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConstraintResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the current request.
-- * 'constraintDetail' - Information about the constraint.
-- * 'constraintParameters' - The constraint parameters.
-- * 'responseStatus' - The response status code.
mkDescribeConstraintResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConstraintResponse
mkDescribeConstraintResponse pResponseStatus_ =
  DescribeConstraintResponse'
    { status = Lude.Nothing,
      constraintDetail = Lude.Nothing,
      constraintParameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsStatus :: Lens.Lens' DescribeConstraintResponse (Lude.Maybe RequestStatus)
dcrsStatus = Lens.lens (status :: DescribeConstraintResponse -> Lude.Maybe RequestStatus) (\s a -> s {status = a} :: DescribeConstraintResponse)
{-# DEPRECATED dcrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Information about the constraint.
--
-- /Note:/ Consider using 'constraintDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsConstraintDetail :: Lens.Lens' DescribeConstraintResponse (Lude.Maybe ConstraintDetail)
dcrsConstraintDetail = Lens.lens (constraintDetail :: DescribeConstraintResponse -> Lude.Maybe ConstraintDetail) (\s a -> s {constraintDetail = a} :: DescribeConstraintResponse)
{-# DEPRECATED dcrsConstraintDetail "Use generic-lens or generic-optics with 'constraintDetail' instead." #-}

-- | The constraint parameters.
--
-- /Note:/ Consider using 'constraintParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsConstraintParameters :: Lens.Lens' DescribeConstraintResponse (Lude.Maybe Lude.Text)
dcrsConstraintParameters = Lens.lens (constraintParameters :: DescribeConstraintResponse -> Lude.Maybe Lude.Text) (\s a -> s {constraintParameters = a} :: DescribeConstraintResponse)
{-# DEPRECATED dcrsConstraintParameters "Use generic-lens or generic-optics with 'constraintParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeConstraintResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeConstraintResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConstraintResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
