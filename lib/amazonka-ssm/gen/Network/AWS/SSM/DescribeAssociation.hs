{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the association for the specified target or instance. If you created the association by using the @Targets@ parameter, then you must retrieve the association by using the association ID. If you created the association by specifying an instance ID and a Systems Manager document, then you retrieve the association by specifying the document name and the instance ID.
module Network.AWS.SSM.DescribeAssociation
  ( -- * Creating a request
    DescribeAssociation (..),
    mkDescribeAssociation,

    -- ** Request lenses
    daAssociationId,
    daInstanceId,
    daName,
    daAssociationVersion,

    -- * Destructuring the response
    DescribeAssociationResponse (..),
    mkDescribeAssociationResponse,

    -- ** Response lenses
    daarsAssociationDescription,
    daarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeAssociation' smart constructor.
data DescribeAssociation = DescribeAssociation'
  { associationId ::
      Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    associationVersion :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID for which you want information.
-- * 'associationVersion' - Specify the association version to retrieve. To view the latest version, either specify @> LATEST@ for this parameter, or omit this parameter. To view a list of all associations for an instance, use 'ListAssociations' . To get a list of versions for a specific association, use 'ListAssociationVersions' .
-- * 'instanceId' - The instance ID.
-- * 'name' - The name of the Systems Manager document.
mkDescribeAssociation ::
  DescribeAssociation
mkDescribeAssociation =
  DescribeAssociation'
    { associationId = Lude.Nothing,
      instanceId = Lude.Nothing,
      name = Lude.Nothing,
      associationVersion = Lude.Nothing
    }

-- | The association ID for which you want information.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAssociationId :: Lens.Lens' DescribeAssociation (Lude.Maybe Lude.Text)
daAssociationId = Lens.lens (associationId :: DescribeAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: DescribeAssociation)
{-# DEPRECATED daAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daInstanceId :: Lens.Lens' DescribeAssociation (Lude.Maybe Lude.Text)
daInstanceId = Lens.lens (instanceId :: DescribeAssociation -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DescribeAssociation)
{-# DEPRECATED daInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daName :: Lens.Lens' DescribeAssociation (Lude.Maybe Lude.Text)
daName = Lens.lens (name :: DescribeAssociation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeAssociation)
{-# DEPRECATED daName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specify the association version to retrieve. To view the latest version, either specify @> LATEST@ for this parameter, or omit this parameter. To view a list of all associations for an instance, use 'ListAssociations' . To get a list of versions for a specific association, use 'ListAssociationVersions' .
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAssociationVersion :: Lens.Lens' DescribeAssociation (Lude.Maybe Lude.Text)
daAssociationVersion = Lens.lens (associationVersion :: DescribeAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: DescribeAssociation)
{-# DEPRECATED daAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

instance Lude.AWSRequest DescribeAssociation where
  type Rs DescribeAssociation = DescribeAssociationResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAssociationResponse'
            Lude.<$> (x Lude..?> "AssociationDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAssociation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeAssociation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAssociation where
  toJSON DescribeAssociation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AssociationId" Lude..=) Lude.<$> associationId,
            ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("Name" Lude..=) Lude.<$> name,
            ("AssociationVersion" Lude..=) Lude.<$> associationVersion
          ]
      )

instance Lude.ToPath DescribeAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAssociation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAssociationResponse' smart constructor.
data DescribeAssociationResponse = DescribeAssociationResponse'
  { associationDescription ::
      Lude.Maybe AssociationDescription,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAssociationResponse' with the minimum fields required to make a request.
--
-- * 'associationDescription' - Information about the association.
-- * 'responseStatus' - The response status code.
mkDescribeAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAssociationResponse
mkDescribeAssociationResponse pResponseStatus_ =
  DescribeAssociationResponse'
    { associationDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the association.
--
-- /Note:/ Consider using 'associationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAssociationDescription :: Lens.Lens' DescribeAssociationResponse (Lude.Maybe AssociationDescription)
daarsAssociationDescription = Lens.lens (associationDescription :: DescribeAssociationResponse -> Lude.Maybe AssociationDescription) (\s a -> s {associationDescription = a} :: DescribeAssociationResponse)
{-# DEPRECATED daarsAssociationDescription "Use generic-lens or generic-optics with 'associationDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResponseStatus :: Lens.Lens' DescribeAssociationResponse Lude.Int
daarsResponseStatus = Lens.lens (responseStatus :: DescribeAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAssociationResponse)
{-# DEPRECATED daarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
