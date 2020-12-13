{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the compiled information from a 'RequestEnvironmentInfo' request.
--
-- Related Topics
--
--     * 'RequestEnvironmentInfo'
module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
  ( -- * Creating a request
    RetrieveEnvironmentInfo (..),
    mkRetrieveEnvironmentInfo,

    -- ** Request lenses
    reiInfoType,
    reiEnvironmentName,
    reiEnvironmentId,

    -- * Destructuring the response
    RetrieveEnvironmentInfoResponse (..),
    mkRetrieveEnvironmentInfoResponse,

    -- ** Response lenses
    reirsEnvironmentInfo,
    reirsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to download logs retrieved with 'RequestEnvironmentInfo' .
--
-- /See:/ 'mkRetrieveEnvironmentInfo' smart constructor.
data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo'
  { -- | The type of information to retrieve.
    infoType :: EnvironmentInfoType,
    -- | The name of the data's environment.
    --
    -- If no such environment is found, returns an @InvalidParameterValue@ error.
    -- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentName :: Lude.Maybe Lude.Text,
    -- | The ID of the data's environment.
    --
    -- If no such environment is found, returns an @InvalidParameterValue@ error.
    -- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetrieveEnvironmentInfo' with the minimum fields required to make a request.
--
-- * 'infoType' - The type of information to retrieve.
-- * 'environmentName' - The name of the data's environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'environmentId' - The ID of the data's environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
mkRetrieveEnvironmentInfo ::
  -- | 'infoType'
  EnvironmentInfoType ->
  RetrieveEnvironmentInfo
mkRetrieveEnvironmentInfo pInfoType_ =
  RetrieveEnvironmentInfo'
    { infoType = pInfoType_,
      environmentName = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | The type of information to retrieve.
--
-- /Note:/ Consider using 'infoType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiInfoType :: Lens.Lens' RetrieveEnvironmentInfo EnvironmentInfoType
reiInfoType = Lens.lens (infoType :: RetrieveEnvironmentInfo -> EnvironmentInfoType) (\s a -> s {infoType = a} :: RetrieveEnvironmentInfo)
{-# DEPRECATED reiInfoType "Use generic-lens or generic-optics with 'infoType' instead." #-}

-- | The name of the data's environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiEnvironmentName :: Lens.Lens' RetrieveEnvironmentInfo (Lude.Maybe Lude.Text)
reiEnvironmentName = Lens.lens (environmentName :: RetrieveEnvironmentInfo -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: RetrieveEnvironmentInfo)
{-# DEPRECATED reiEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The ID of the data's environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiEnvironmentId :: Lens.Lens' RetrieveEnvironmentInfo (Lude.Maybe Lude.Text)
reiEnvironmentId = Lens.lens (environmentId :: RetrieveEnvironmentInfo -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: RetrieveEnvironmentInfo)
{-# DEPRECATED reiEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest RetrieveEnvironmentInfo where
  type Rs RetrieveEnvironmentInfo = RetrieveEnvironmentInfoResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "RetrieveEnvironmentInfoResult"
      ( \s h x ->
          RetrieveEnvironmentInfoResponse'
            Lude.<$> ( x Lude..@? "EnvironmentInfo" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RetrieveEnvironmentInfo where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RetrieveEnvironmentInfo where
  toPath = Lude.const "/"

instance Lude.ToQuery RetrieveEnvironmentInfo where
  toQuery RetrieveEnvironmentInfo' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RetrieveEnvironmentInfo" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "InfoType" Lude.=: infoType,
        "EnvironmentName" Lude.=: environmentName,
        "EnvironmentId" Lude.=: environmentId
      ]

-- | Result message containing a description of the requested environment info.
--
-- /See:/ 'mkRetrieveEnvironmentInfoResponse' smart constructor.
data RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse'
  { -- | The 'EnvironmentInfoDescription' of the environment.
    environmentInfo :: Lude.Maybe [EnvironmentInfoDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetrieveEnvironmentInfoResponse' with the minimum fields required to make a request.
--
-- * 'environmentInfo' - The 'EnvironmentInfoDescription' of the environment.
-- * 'responseStatus' - The response status code.
mkRetrieveEnvironmentInfoResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RetrieveEnvironmentInfoResponse
mkRetrieveEnvironmentInfoResponse pResponseStatus_ =
  RetrieveEnvironmentInfoResponse'
    { environmentInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'EnvironmentInfoDescription' of the environment.
--
-- /Note:/ Consider using 'environmentInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reirsEnvironmentInfo :: Lens.Lens' RetrieveEnvironmentInfoResponse (Lude.Maybe [EnvironmentInfoDescription])
reirsEnvironmentInfo = Lens.lens (environmentInfo :: RetrieveEnvironmentInfoResponse -> Lude.Maybe [EnvironmentInfoDescription]) (\s a -> s {environmentInfo = a} :: RetrieveEnvironmentInfoResponse)
{-# DEPRECATED reirsEnvironmentInfo "Use generic-lens or generic-optics with 'environmentInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reirsResponseStatus :: Lens.Lens' RetrieveEnvironmentInfoResponse Lude.Int
reirsResponseStatus = Lens.lens (responseStatus :: RetrieveEnvironmentInfoResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RetrieveEnvironmentInfoResponse)
{-# DEPRECATED reirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
