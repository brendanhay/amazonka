{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeBrokerEngineTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available engine types and versions.
module Network.AWS.MQ.DescribeBrokerEngineTypes
  ( -- * Creating a request
    DescribeBrokerEngineTypes (..),
    mkDescribeBrokerEngineTypes,

    -- ** Request lenses
    dbetNextToken,
    dbetEngineType,
    dbetMaxResults,

    -- * Destructuring the response
    DescribeBrokerEngineTypesResponse (..),
    mkDescribeBrokerEngineTypesResponse,

    -- ** Response lenses
    dbetrsBrokerEngineTypes,
    dbetrsNextToken,
    dbetrsMaxResults,
    dbetrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBrokerEngineTypes' smart constructor.
data DescribeBrokerEngineTypes = DescribeBrokerEngineTypes'
  { nextToken ::
      Lude.Maybe Lude.Text,
    engineType :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBrokerEngineTypes' with the minimum fields required to make a request.
--
-- * 'engineType' - Filter response by engine type.
-- * 'maxResults' - The maximum number of engine types that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
mkDescribeBrokerEngineTypes ::
  DescribeBrokerEngineTypes
mkDescribeBrokerEngineTypes =
  DescribeBrokerEngineTypes'
    { nextToken = Lude.Nothing,
      engineType = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetNextToken :: Lens.Lens' DescribeBrokerEngineTypes (Lude.Maybe Lude.Text)
dbetNextToken = Lens.lens (nextToken :: DescribeBrokerEngineTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBrokerEngineTypes)
{-# DEPRECATED dbetNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filter response by engine type.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetEngineType :: Lens.Lens' DescribeBrokerEngineTypes (Lude.Maybe Lude.Text)
dbetEngineType = Lens.lens (engineType :: DescribeBrokerEngineTypes -> Lude.Maybe Lude.Text) (\s a -> s {engineType = a} :: DescribeBrokerEngineTypes)
{-# DEPRECATED dbetEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | The maximum number of engine types that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetMaxResults :: Lens.Lens' DescribeBrokerEngineTypes (Lude.Maybe Lude.Natural)
dbetMaxResults = Lens.lens (maxResults :: DescribeBrokerEngineTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBrokerEngineTypes)
{-# DEPRECATED dbetMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeBrokerEngineTypes where
  type
    Rs DescribeBrokerEngineTypes =
      DescribeBrokerEngineTypesResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBrokerEngineTypesResponse'
            Lude.<$> (x Lude..?> "brokerEngineTypes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "maxResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBrokerEngineTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeBrokerEngineTypes where
  toPath = Lude.const "/v1/broker-engine-types"

instance Lude.ToQuery DescribeBrokerEngineTypes where
  toQuery DescribeBrokerEngineTypes' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "engineType" Lude.=: engineType,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeBrokerEngineTypesResponse' smart constructor.
data DescribeBrokerEngineTypesResponse = DescribeBrokerEngineTypesResponse'
  { brokerEngineTypes ::
      Lude.Maybe
        [BrokerEngineType],
    nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBrokerEngineTypesResponse' with the minimum fields required to make a request.
--
-- * 'brokerEngineTypes' - List of available engine types and versions.
-- * 'maxResults' - Required. The maximum number of engine types that can be returned per page (20 by default). This value must be an integer from 5 to 100.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
-- * 'responseStatus' - The response status code.
mkDescribeBrokerEngineTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBrokerEngineTypesResponse
mkDescribeBrokerEngineTypesResponse pResponseStatus_ =
  DescribeBrokerEngineTypesResponse'
    { brokerEngineTypes =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of available engine types and versions.
--
-- /Note:/ Consider using 'brokerEngineTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetrsBrokerEngineTypes :: Lens.Lens' DescribeBrokerEngineTypesResponse (Lude.Maybe [BrokerEngineType])
dbetrsBrokerEngineTypes = Lens.lens (brokerEngineTypes :: DescribeBrokerEngineTypesResponse -> Lude.Maybe [BrokerEngineType]) (\s a -> s {brokerEngineTypes = a} :: DescribeBrokerEngineTypesResponse)
{-# DEPRECATED dbetrsBrokerEngineTypes "Use generic-lens or generic-optics with 'brokerEngineTypes' instead." #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetrsNextToken :: Lens.Lens' DescribeBrokerEngineTypesResponse (Lude.Maybe Lude.Text)
dbetrsNextToken = Lens.lens (nextToken :: DescribeBrokerEngineTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBrokerEngineTypesResponse)
{-# DEPRECATED dbetrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Required. The maximum number of engine types that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetrsMaxResults :: Lens.Lens' DescribeBrokerEngineTypesResponse (Lude.Maybe Lude.Natural)
dbetrsMaxResults = Lens.lens (maxResults :: DescribeBrokerEngineTypesResponse -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBrokerEngineTypesResponse)
{-# DEPRECATED dbetrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbetrsResponseStatus :: Lens.Lens' DescribeBrokerEngineTypesResponse Lude.Int
dbetrsResponseStatus = Lens.lens (responseStatus :: DescribeBrokerEngineTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBrokerEngineTypesResponse)
{-# DEPRECATED dbetrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
