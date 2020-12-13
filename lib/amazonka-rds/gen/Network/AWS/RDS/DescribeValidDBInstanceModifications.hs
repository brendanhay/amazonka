{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeValidDBInstanceModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can call @DescribeValidDBInstanceModifications@ to learn what modifications you can make to your DB instance. You can use this information when you call @ModifyDBInstance@ .
module Network.AWS.RDS.DescribeValidDBInstanceModifications
  ( -- * Creating a request
    DescribeValidDBInstanceModifications (..),
    mkDescribeValidDBInstanceModifications,

    -- ** Request lenses
    dvdimDBInstanceIdentifier,

    -- * Destructuring the response
    DescribeValidDBInstanceModificationsResponse (..),
    mkDescribeValidDBInstanceModificationsResponse,

    -- ** Response lenses
    dvdimrsValidDBInstanceModificationsMessage,
    dvdimrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeValidDBInstanceModifications' smart constructor.
newtype DescribeValidDBInstanceModifications = DescribeValidDBInstanceModifications'
  { -- | The customer identifier or the ARN of your DB instance.
    dbInstanceIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeValidDBInstanceModifications' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The customer identifier or the ARN of your DB instance.
mkDescribeValidDBInstanceModifications ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  DescribeValidDBInstanceModifications
mkDescribeValidDBInstanceModifications pDBInstanceIdentifier_ =
  DescribeValidDBInstanceModifications'
    { dbInstanceIdentifier =
        pDBInstanceIdentifier_
    }

-- | The customer identifier or the ARN of your DB instance.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvdimDBInstanceIdentifier :: Lens.Lens' DescribeValidDBInstanceModifications Lude.Text
dvdimDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DescribeValidDBInstanceModifications -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DescribeValidDBInstanceModifications)
{-# DEPRECATED dvdimDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

instance Lude.AWSRequest DescribeValidDBInstanceModifications where
  type
    Rs DescribeValidDBInstanceModifications =
      DescribeValidDBInstanceModificationsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeValidDBInstanceModificationsResult"
      ( \s h x ->
          DescribeValidDBInstanceModificationsResponse'
            Lude.<$> (x Lude..@? "ValidDBInstanceModificationsMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeValidDBInstanceModifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeValidDBInstanceModifications where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeValidDBInstanceModifications where
  toQuery DescribeValidDBInstanceModifications' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeValidDBInstanceModifications" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier
      ]

-- | /See:/ 'mkDescribeValidDBInstanceModificationsResponse' smart constructor.
data DescribeValidDBInstanceModificationsResponse = DescribeValidDBInstanceModificationsResponse'
  { validDBInstanceModificationsMessage :: Lude.Maybe ValidDBInstanceModificationsMessage,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeValidDBInstanceModificationsResponse' with the minimum fields required to make a request.
--
-- * 'validDBInstanceModificationsMessage' -
-- * 'responseStatus' - The response status code.
mkDescribeValidDBInstanceModificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeValidDBInstanceModificationsResponse
mkDescribeValidDBInstanceModificationsResponse pResponseStatus_ =
  DescribeValidDBInstanceModificationsResponse'
    { validDBInstanceModificationsMessage =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'validDBInstanceModificationsMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvdimrsValidDBInstanceModificationsMessage :: Lens.Lens' DescribeValidDBInstanceModificationsResponse (Lude.Maybe ValidDBInstanceModificationsMessage)
dvdimrsValidDBInstanceModificationsMessage = Lens.lens (validDBInstanceModificationsMessage :: DescribeValidDBInstanceModificationsResponse -> Lude.Maybe ValidDBInstanceModificationsMessage) (\s a -> s {validDBInstanceModificationsMessage = a} :: DescribeValidDBInstanceModificationsResponse)
{-# DEPRECATED dvdimrsValidDBInstanceModificationsMessage "Use generic-lens or generic-optics with 'validDBInstanceModificationsMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvdimrsResponseStatus :: Lens.Lens' DescribeValidDBInstanceModificationsResponse Lude.Int
dvdimrsResponseStatus = Lens.lens (responseStatus :: DescribeValidDBInstanceModificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeValidDBInstanceModificationsResponse)
{-# DEPRECATED dvdimrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
