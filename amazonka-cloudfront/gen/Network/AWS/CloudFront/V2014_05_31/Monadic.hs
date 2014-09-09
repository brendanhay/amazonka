{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudFront is a web service that speeds up distribution of your
-- static and dynamic web content, for example, .html, .css, .php, image, and
-- media files, to end users. CloudFront delivers your content through a
-- worldwide network of edge locations. When an end user requests content that
-- you're serving with CloudFront, the user is routed to the edge location
-- that provides the lowest latency, so content is delivered with the best
-- possible performance. If the content is already in that edge location,
-- CloudFront delivers it immediately. If the content is not currently in that
-- edge location, CloudFront retrieves it from an Amazon S3 bucket or an HTTP
-- server (for example, a web server) that you have identified as the source
-- for the definitive version of your content.
--
-- The 'State' operator variants from "Control.Lens.Setter" such as '.='
-- can be used to modify any additional request parameters before sending.
module Network.AWS.CloudFront.V2014_05_31.Monadic
    (
    -- * CreateCloudFrontOriginAccessIdentity
    -- $CreateCloudFrontOriginAccessIdentity
      createCloudFrontOriginAccessIdentity
    , createCloudFrontOriginAccessIdentityCatch

    -- * CreateDistribution
    -- $CreateDistribution
    , createDistribution
    , createDistributionCatch

    -- * CreateInvalidation
    -- $CreateInvalidation
    , createInvalidation
    , createInvalidationCatch

    -- * CreateStreamingDistribution
    -- $CreateStreamingDistribution
    , createStreamingDistribution
    , createStreamingDistributionCatch

    -- * DeleteCloudFrontOriginAccessIdentity
    -- $DeleteCloudFrontOriginAccessIdentity
    , deleteCloudFrontOriginAccessIdentity
    , deleteCloudFrontOriginAccessIdentityCatch

    -- * DeleteDistribution
    -- $DeleteDistribution
    , deleteDistribution
    , deleteDistributionCatch

    -- * DeleteStreamingDistribution
    -- $DeleteStreamingDistribution
    , deleteStreamingDistribution
    , deleteStreamingDistributionCatch

    -- * GetCloudFrontOriginAccessIdentity
    -- $GetCloudFrontOriginAccessIdentity
    , getCloudFrontOriginAccessIdentity
    , getCloudFrontOriginAccessIdentityCatch

    -- * GetCloudFrontOriginAccessIdentityConfig
    -- $GetCloudFrontOriginAccessIdentityConfig
    , getCloudFrontOriginAccessIdentityConfig
    , getCloudFrontOriginAccessIdentityConfigCatch

    -- * GetDistribution
    -- $GetDistribution
    , getDistribution
    , getDistributionCatch

    -- * GetDistributionConfig
    -- $GetDistributionConfig
    , getDistributionConfig
    , getDistributionConfigCatch

    -- * GetInvalidation
    -- $GetInvalidation
    , getInvalidation
    , getInvalidationCatch

    -- * GetStreamingDistribution
    -- $GetStreamingDistribution
    , getStreamingDistribution
    , getStreamingDistributionCatch

    -- * GetStreamingDistributionConfig
    -- $GetStreamingDistributionConfig
    , getStreamingDistributionConfig
    , getStreamingDistributionConfigCatch

    -- * ListCloudFrontOriginAccessIdentities
    -- $ListCloudFrontOriginAccessIdentities
    , listCloudFrontOriginAccessIdentities
    , listCloudFrontOriginAccessIdentitiesCatch

    -- * ListDistributions
    -- $ListDistributions
    , listDistributions
    , listDistributionsCatch

    -- * ListInvalidations
    -- $ListInvalidations
    , listInvalidations
    , listInvalidationsCatch

    -- * ListStreamingDistributions
    -- $ListStreamingDistributions
    , listStreamingDistributions
    , listStreamingDistributionsCatch

    -- * UpdateCloudFrontOriginAccessIdentity
    -- $UpdateCloudFrontOriginAccessIdentity
    , updateCloudFrontOriginAccessIdentity
    , updateCloudFrontOriginAccessIdentityCatch

    -- * UpdateDistribution
    -- $UpdateDistribution
    , updateDistribution
    , updateDistributionCatch

    -- * UpdateStreamingDistribution
    -- $UpdateStreamingDistribution
    , updateStreamingDistribution
    , updateStreamingDistributionCatch

    -- * Re-exported
    , module Network.AWS.CloudFront.V2014_05_31

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.CloudFront.V2014_05_31

type ServiceEr = Er CloudFront


-- $CreateCloudFrontOriginAccessIdentity
-- Create a new origin access identity.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity'

createCloudFrontOriginAccessIdentity :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadError AWS.Error m
                                        , MonadReader Env m
                                        )
    => CloudFrontOriginAccessIdentityConfig -- ^ 'ccfoaiCloudFrontOriginAccessIdentityConfig'
    -> State CreateCloudFrontOriginAccessIdentity a
    -> m CreateCloudFrontOriginAccessIdentityResponse
createCloudFrontOriginAccessIdentity p1 s =
    send $ (mkCreateCloudFrontOriginAccessIdentity p1) &~ s

createCloudFrontOriginAccessIdentityCatch :: ( MonadCatch m
                                             , MonadResource m
                                             , MonadReader Env m
                                             )
    => CloudFrontOriginAccessIdentityConfig -- ^ 'ccfoaiCloudFrontOriginAccessIdentityConfig'
    -> State CreateCloudFrontOriginAccessIdentity a
    -> m (Either ServiceEr CreateCloudFrontOriginAccessIdentityResponse)
createCloudFrontOriginAccessIdentityCatch p1 s =
    sendCatch $ (mkCreateCloudFrontOriginAccessIdentity p1) &~ s

-- $CreateDistribution
-- Create a new distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.CreateDistribution'

createDistribution :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => DistributionConfig -- ^ 'cdDistributionConfig'
    -> State CreateDistribution a
    -> m CreateDistributionResponse
createDistribution p1 s =
    send $ (mkCreateDistribution p1) &~ s

createDistributionCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => DistributionConfig -- ^ 'cdDistributionConfig'
    -> State CreateDistribution a
    -> m (Either ServiceEr CreateDistributionResponse)
createDistributionCatch p1 s =
    sendCatch $ (mkCreateDistribution p1) &~ s

-- $CreateInvalidation
-- Create a new invalidation.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.CreateInvalidation'

createInvalidation :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'ciDistributionId'
    -> InvalidationBatch -- ^ 'ciInvalidationBatch'
    -> State CreateInvalidation a
    -> m CreateInvalidationResponse
createInvalidation p1 p2 s =
    send $ (mkCreateInvalidation p1 p2) &~ s

createInvalidationCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'ciDistributionId'
    -> InvalidationBatch -- ^ 'ciInvalidationBatch'
    -> State CreateInvalidation a
    -> m (Either ServiceEr CreateInvalidationResponse)
createInvalidationCatch p1 p2 s =
    sendCatch $ (mkCreateInvalidation p1 p2) &~ s

-- $CreateStreamingDistribution
-- Create a new streaming distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution'

createStreamingDistribution :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => StreamingDistributionConfig -- ^ 'csdStreamingDistributionConfig'
    -> State CreateStreamingDistribution a
    -> m CreateStreamingDistributionResponse
createStreamingDistribution p1 s =
    send $ (mkCreateStreamingDistribution p1) &~ s

createStreamingDistributionCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => StreamingDistributionConfig -- ^ 'csdStreamingDistributionConfig'
    -> State CreateStreamingDistribution a
    -> m (Either ServiceEr CreateStreamingDistributionResponse)
createStreamingDistributionCatch p1 s =
    sendCatch $ (mkCreateStreamingDistribution p1) &~ s

-- $DeleteCloudFrontOriginAccessIdentity
-- Delete an origin access identity.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity'

deleteCloudFrontOriginAccessIdentity :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadError AWS.Error m
                                        , MonadReader Env m
                                        )
    => Text -- ^ 'dcfoaiId'
    -> State DeleteCloudFrontOriginAccessIdentity a
    -> m DeleteCloudFrontOriginAccessIdentityResponse
deleteCloudFrontOriginAccessIdentity p1 s =
    send $ (mkDeleteCloudFrontOriginAccessIdentity p1) &~ s

deleteCloudFrontOriginAccessIdentityCatch :: ( MonadCatch m
                                             , MonadResource m
                                             , MonadReader Env m
                                             )
    => Text -- ^ 'dcfoaiId'
    -> State DeleteCloudFrontOriginAccessIdentity a
    -> m (Either ServiceEr DeleteCloudFrontOriginAccessIdentityResponse)
deleteCloudFrontOriginAccessIdentityCatch p1 s =
    sendCatch $ (mkDeleteCloudFrontOriginAccessIdentity p1) &~ s

-- $DeleteDistribution
-- Delete a distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.DeleteDistribution'

deleteDistribution :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'ddId'
    -> State DeleteDistribution a
    -> m DeleteDistributionResponse
deleteDistribution p1 s =
    send $ (mkDeleteDistribution p1) &~ s

deleteDistributionCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'ddId'
    -> State DeleteDistribution a
    -> m (Either ServiceEr DeleteDistributionResponse)
deleteDistributionCatch p1 s =
    sendCatch $ (mkDeleteDistribution p1) &~ s

-- $DeleteStreamingDistribution
-- Delete a streaming distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution'

deleteStreamingDistribution :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'dsdId'
    -> State DeleteStreamingDistribution a
    -> m DeleteStreamingDistributionResponse
deleteStreamingDistribution p1 s =
    send $ (mkDeleteStreamingDistribution p1) &~ s

deleteStreamingDistributionCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'dsdId'
    -> State DeleteStreamingDistribution a
    -> m (Either ServiceEr DeleteStreamingDistributionResponse)
deleteStreamingDistributionCatch p1 s =
    sendCatch $ (mkDeleteStreamingDistribution p1) &~ s

-- $GetCloudFrontOriginAccessIdentity
-- Get the information about an origin access identity.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity'

getCloudFrontOriginAccessIdentity :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'gcfoaiId'
    -> State GetCloudFrontOriginAccessIdentity a
    -> m GetCloudFrontOriginAccessIdentityResponse
getCloudFrontOriginAccessIdentity p1 s =
    send $ (mkGetCloudFrontOriginAccessIdentity p1) &~ s

getCloudFrontOriginAccessIdentityCatch :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadReader Env m
                                          )
    => Text -- ^ 'gcfoaiId'
    -> State GetCloudFrontOriginAccessIdentity a
    -> m (Either ServiceEr GetCloudFrontOriginAccessIdentityResponse)
getCloudFrontOriginAccessIdentityCatch p1 s =
    sendCatch $ (mkGetCloudFrontOriginAccessIdentity p1) &~ s

-- $GetCloudFrontOriginAccessIdentityConfig
-- Get the configuration information about an origin access identity.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig'

getCloudFrontOriginAccessIdentityConfig :: ( MonadCatch m
                                           , MonadResource m
                                           , MonadError AWS.Error m
                                           , MonadReader Env m
                                           )
    => Text -- ^ 'gcfoaicId'
    -> State GetCloudFrontOriginAccessIdentityConfig a
    -> m GetCloudFrontOriginAccessIdentityConfigResponse
getCloudFrontOriginAccessIdentityConfig p1 s =
    send $ (mkGetCloudFrontOriginAccessIdentityConfig p1) &~ s

getCloudFrontOriginAccessIdentityConfigCatch :: ( MonadCatch m
                                                , MonadResource m
                                                , MonadReader Env m
                                                )
    => Text -- ^ 'gcfoaicId'
    -> State GetCloudFrontOriginAccessIdentityConfig a
    -> m (Either ServiceEr GetCloudFrontOriginAccessIdentityConfigResponse)
getCloudFrontOriginAccessIdentityConfigCatch p1 s =
    sendCatch $ (mkGetCloudFrontOriginAccessIdentityConfig p1) &~ s

-- $GetDistribution
-- Get the information about a distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.GetDistribution'

getDistribution :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'gdId'
    -> State GetDistribution a
    -> m GetDistributionResponse
getDistribution p1 s =
    send $ (mkGetDistribution p1) &~ s

getDistributionCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'gdId'
    -> State GetDistribution a
    -> m (Either ServiceEr GetDistributionResponse)
getDistributionCatch p1 s =
    sendCatch $ (mkGetDistribution p1) &~ s

-- $GetDistributionConfig
-- Get the configuration information about a distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig'

getDistributionConfig :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'gdcId'
    -> State GetDistributionConfig a
    -> m GetDistributionConfigResponse
getDistributionConfig p1 s =
    send $ (mkGetDistributionConfig p1) &~ s

getDistributionConfigCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'gdcId'
    -> State GetDistributionConfig a
    -> m (Either ServiceEr GetDistributionConfigResponse)
getDistributionConfigCatch p1 s =
    sendCatch $ (mkGetDistributionConfig p1) &~ s

-- $GetInvalidation
-- Get the information about an invalidation.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.GetInvalidation'

getInvalidation :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'giDistributionId'
    -> Text -- ^ 'giId'
    -> State GetInvalidation a
    -> m GetInvalidationResponse
getInvalidation p1 p2 s =
    send $ (mkGetInvalidation p1 p2) &~ s

getInvalidationCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'giDistributionId'
    -> Text -- ^ 'giId'
    -> State GetInvalidation a
    -> m (Either ServiceEr GetInvalidationResponse)
getInvalidationCatch p1 p2 s =
    sendCatch $ (mkGetInvalidation p1 p2) &~ s

-- $GetStreamingDistribution
-- Get the information about a streaming distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution'

getStreamingDistribution :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'gsdId'
    -> State GetStreamingDistribution a
    -> m GetStreamingDistributionResponse
getStreamingDistribution p1 s =
    send $ (mkGetStreamingDistribution p1) &~ s

getStreamingDistributionCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'gsdId'
    -> State GetStreamingDistribution a
    -> m (Either ServiceEr GetStreamingDistributionResponse)
getStreamingDistributionCatch p1 s =
    sendCatch $ (mkGetStreamingDistribution p1) &~ s

-- $GetStreamingDistributionConfig
-- Get the configuration information about a streaming distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig'

getStreamingDistributionConfig :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'gsdcId'
    -> State GetStreamingDistributionConfig a
    -> m GetStreamingDistributionConfigResponse
getStreamingDistributionConfig p1 s =
    send $ (mkGetStreamingDistributionConfig p1) &~ s

getStreamingDistributionConfigCatch :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadReader Env m
                                       )
    => Text -- ^ 'gsdcId'
    -> State GetStreamingDistributionConfig a
    -> m (Either ServiceEr GetStreamingDistributionConfigResponse)
getStreamingDistributionConfigCatch p1 s =
    sendCatch $ (mkGetStreamingDistributionConfig p1) &~ s

-- $ListCloudFrontOriginAccessIdentities
-- List origin access identities.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities'

listCloudFrontOriginAccessIdentities :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadError AWS.Error m
                                        , MonadReader Env (ResumableSource m)
                                        )
    => State ListCloudFrontOriginAccessIdentities a
    -> ResumableSource m ListCloudFrontOriginAccessIdentitiesResponse
listCloudFrontOriginAccessIdentities s =
    paginate (mkListCloudFrontOriginAccessIdentities &~ s)

listCloudFrontOriginAccessIdentitiesCatch :: ( MonadCatch m
                                             , MonadResource m
                                             , MonadReader Env (ResumableSource m)
                                             )
    => State ListCloudFrontOriginAccessIdentities a
    -> ResumableSource m (Either ServiceEr ListCloudFrontOriginAccessIdentitiesResponse)
listCloudFrontOriginAccessIdentitiesCatch s =
    paginateCatch (mkListCloudFrontOriginAccessIdentities &~ s)

-- $ListDistributions
-- List distributions.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.ListDistributions'

listDistributions :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env (ResumableSource m)
                     )
    => State ListDistributions a
    -> ResumableSource m ListDistributionsResponse
listDistributions s =
    paginate (mkListDistributions &~ s)

listDistributionsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env (ResumableSource m)
                          )
    => State ListDistributions a
    -> ResumableSource m (Either ServiceEr ListDistributionsResponse)
listDistributionsCatch s =
    paginateCatch (mkListDistributions &~ s)

-- $ListInvalidations
-- List invalidation batches.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.ListInvalidations'

listInvalidations :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env (ResumableSource m)
                     )
    => Text -- ^ 'liDistributionId'
    -> State ListInvalidations a
    -> ResumableSource m ListInvalidationsResponse
listInvalidations p1 s =
    paginate $ (mkListInvalidations p1) &~ s

listInvalidationsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env (ResumableSource m)
                          )
    => Text -- ^ 'liDistributionId'
    -> State ListInvalidations a
    -> ResumableSource m (Either ServiceEr ListInvalidationsResponse)
listInvalidationsCatch p1 s =
    paginateCatch $ (mkListInvalidations p1) &~ s

-- $ListStreamingDistributions
-- List streaming distributions.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions'

listStreamingDistributions :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env (ResumableSource m)
                              )
    => State ListStreamingDistributions a
    -> ResumableSource m ListStreamingDistributionsResponse
listStreamingDistributions s =
    paginate (mkListStreamingDistributions &~ s)

listStreamingDistributionsCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env (ResumableSource m)
                                   )
    => State ListStreamingDistributions a
    -> ResumableSource m (Either ServiceEr ListStreamingDistributionsResponse)
listStreamingDistributionsCatch s =
    paginateCatch (mkListStreamingDistributions &~ s)

-- $UpdateCloudFrontOriginAccessIdentity
-- Update an origin access identity.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity'

updateCloudFrontOriginAccessIdentity :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadError AWS.Error m
                                        , MonadReader Env m
                                        )
    => CloudFrontOriginAccessIdentityConfig -- ^ 'ucfoaiCloudFrontOriginAccessIdentityConfig'
    -> Text -- ^ 'ucfoaiId'
    -> State UpdateCloudFrontOriginAccessIdentity a
    -> m UpdateCloudFrontOriginAccessIdentityResponse
updateCloudFrontOriginAccessIdentity p1 p2 s =
    send $ (mkUpdateCloudFrontOriginAccessIdentity p1 p2) &~ s

updateCloudFrontOriginAccessIdentityCatch :: ( MonadCatch m
                                             , MonadResource m
                                             , MonadReader Env m
                                             )
    => CloudFrontOriginAccessIdentityConfig -- ^ 'ucfoaiCloudFrontOriginAccessIdentityConfig'
    -> Text -- ^ 'ucfoaiId'
    -> State UpdateCloudFrontOriginAccessIdentity a
    -> m (Either ServiceEr UpdateCloudFrontOriginAccessIdentityResponse)
updateCloudFrontOriginAccessIdentityCatch p1 p2 s =
    sendCatch $ (mkUpdateCloudFrontOriginAccessIdentity p1 p2) &~ s

-- $UpdateDistribution
-- Update a distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.UpdateDistribution'

updateDistribution :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => DistributionConfig -- ^ 'udDistributionConfig'
    -> Text -- ^ 'udId'
    -> State UpdateDistribution a
    -> m UpdateDistributionResponse
updateDistribution p1 p2 s =
    send $ (mkUpdateDistribution p1 p2) &~ s

updateDistributionCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => DistributionConfig -- ^ 'udDistributionConfig'
    -> Text -- ^ 'udId'
    -> State UpdateDistribution a
    -> m (Either ServiceEr UpdateDistributionResponse)
updateDistributionCatch p1 p2 s =
    sendCatch $ (mkUpdateDistribution p1 p2) &~ s

-- $UpdateStreamingDistribution
-- Update a streaming distribution.
--
-- See: 'Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution'

updateStreamingDistribution :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => StreamingDistributionConfig -- ^ 'usdStreamingDistributionConfig'
    -> Text -- ^ 'usdId'
    -> State UpdateStreamingDistribution a
    -> m UpdateStreamingDistributionResponse
updateStreamingDistribution p1 p2 s =
    send $ (mkUpdateStreamingDistribution p1 p2) &~ s

updateStreamingDistributionCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => StreamingDistributionConfig -- ^ 'usdStreamingDistributionConfig'
    -> Text -- ^ 'usdId'
    -> State UpdateStreamingDistribution a
    -> m (Either ServiceEr UpdateStreamingDistributionResponse)
updateStreamingDistributionCatch p1 p2 s =
    sendCatch $ (mkUpdateStreamingDistribution p1 p2) &~ s
