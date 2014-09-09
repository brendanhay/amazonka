{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudSearch is a fully-managed service in the AWS Cloud that makes
-- it simple and cost-effective to set up, manage, and scale a search solution
-- for your website or application. Amazon CloudSearch supports 34 languages
-- and popular search features such as highlighting, autocomplete, and
-- geospatial search.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.CloudSearch.V2013_01_01.Trans
    (
    -- * BuildSuggesters
      buildSuggesters
    -- * CreateDomain
    , createDomain
    -- * DefineAnalysisScheme
    , defineAnalysisScheme
    -- * DefineExpression
    , defineExpression
    -- * DefineIndexField
    , defineIndexField
    -- * DefineSuggester
    , defineSuggester
    -- * DeleteAnalysisScheme
    , deleteAnalysisScheme
    -- * DeleteDomain
    , deleteDomain
    -- * DeleteExpression
    , deleteExpression
    -- * DeleteIndexField
    , deleteIndexField
    -- * DeleteSuggester
    , deleteSuggester
    -- * DescribeAnalysisSchemes
    , describeAnalysisSchemes
    -- * DescribeAvailabilityOptions
    , describeAvailabilityOptions
    -- * DescribeDomains
    , describeDomains
    -- * DescribeExpressions
    , describeExpressions
    -- * DescribeIndexFields
    , describeIndexFields
    -- * DescribeScalingParameters
    , describeScalingParameters
    -- * DescribeServiceAccessPolicies
    , describeServiceAccessPolicies
    -- * DescribeSuggesters
    , describeSuggesters
    -- * IndexDocuments
    , indexDocuments
    -- * ListDomainNames
    , listDomainNames
    -- * UpdateAvailabilityOptions
    , updateAvailabilityOptions
    -- * UpdateScalingParameters
    , updateScalingParameters
    -- * UpdateServiceAccessPolicies
    , updateServiceAccessPolicies

    -- * Re-exported
    , module Control.Monad.Trans.AWS
    , module Network.AWS.CloudSearch.V2013_01_01
    -- ** Lenses
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.CloudSearch.V2013_01_01

-- | Indexes the search suggestions.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.BuildSuggesters'
buildSuggesters :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'bsDomainName'
                -> State BuildSuggesters a
                -> m BuildSuggestersResponse
buildSuggesters p1 s =
    send $ (mkBuildSuggesters p1) &~ s

-- | Creates a new search domain. For more information, see Creating a Search
-- Domain in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.CreateDomain'
createDomain :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => Text -- ^ 'cdDomainName'
             -> State CreateDomain a
             -> m CreateDomainResponse
createDomain p1 s =
    send $ (mkCreateDomain p1) &~ s

-- | Configures an analysis scheme that can be applied to a text or text-array
-- field to define language-specific text processing options. For more
-- information, see Configuring Analysis Schemes in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DefineAnalysisScheme'
defineAnalysisScheme :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => Text -- ^ 'dasDomainName'
                     -> AnalysisScheme -- ^ 'dasAnalysisScheme'
                     -> State DefineAnalysisScheme a
                     -> m DefineAnalysisSchemeResponse
defineAnalysisScheme p1 p2 s =
    send $ (mkDefineAnalysisScheme p1 p2) &~ s

-- | Configures an Expression for the search domain. Used to create new
-- expressions and modify existing ones. If the expression exists, the new
-- configuration replaces the old one. For more information, see Configuring
-- Expressions in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DefineExpression'
defineExpression :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'deDomainName'
                 -> Expression -- ^ 'deExpression'
                 -> State DefineExpression a
                 -> m DefineExpressionResponse
defineExpression p1 p2 s =
    send $ (mkDefineExpression p1 p2) &~ s

-- | Configures an IndexField for the search domain. Used to create new fields
-- and modify existing ones. You must specify the name of the domain you are
-- configuring and an index field configuration. The index field configuration
-- specifies a unique name, the index field type, and the options you want to
-- configure for the field. The options you can specify depend on the
-- IndexFieldType. If the field exists, the new configuration replaces the old
-- one. For more information, see Configuring Index Fields in the Amazon
-- CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DefineIndexField'
defineIndexField :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'difDomainName'
                 -> IndexField -- ^ 'difIndexField'
                 -> State DefineIndexField a
                 -> m DefineIndexFieldResponse
defineIndexField p1 p2 s =
    send $ (mkDefineIndexField p1 p2) &~ s

-- | Configures a suggester for a domain. A suggester enables you to display
-- possible matches before users finish typing their queries. When you
-- configure a suggester, you must specify the name of the text field you want
-- to search for possible matches and a unique name for the suggester. For
-- more information, see Getting Search Suggestions in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DefineSuggester'
defineSuggester :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'ds1DomainName'
                -> Suggester -- ^ 'ds1Suggester'
                -> State DefineSuggester a
                -> m DefineSuggesterResponse
defineSuggester p1 p2 s =
    send $ (mkDefineSuggester p1 p2) &~ s

-- | Deletes an analysis scheme. For more information, see Configuring Analysis
-- Schemes in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DeleteAnalysisScheme'
deleteAnalysisScheme :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => Text -- ^ 'das1DomainName'
                     -> Text -- ^ 'das1AnalysisSchemeName'
                     -> State DeleteAnalysisScheme a
                     -> m DeleteAnalysisSchemeResponse
deleteAnalysisScheme p1 p2 s =
    send $ (mkDeleteAnalysisScheme p1 p2) &~ s

-- | Permanently deletes a search domain and all of its data. Once a domain has
-- been deleted, it cannot be recovered. For more information, see Deleting a
-- Search Domain in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DeleteDomain'
deleteDomain :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => Text -- ^ 'ddDomainName'
             -> State DeleteDomain a
             -> m DeleteDomainResponse
deleteDomain p1 s =
    send $ (mkDeleteDomain p1) &~ s

-- | Removes an Expression from the search domain. For more information, see
-- Configuring Expressions in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DeleteExpression'
deleteExpression :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'de1DomainName'
                 -> Text -- ^ 'de1ExpressionName'
                 -> State DeleteExpression a
                 -> m DeleteExpressionResponse
deleteExpression p1 p2 s =
    send $ (mkDeleteExpression p1 p2) &~ s

-- | Removes an IndexField from the search domain. For more information, see
-- Configuring Index Fields in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DeleteIndexField'
deleteIndexField :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'dif1DomainName'
                 -> Text -- ^ 'dif1IndexFieldName'
                 -> State DeleteIndexField a
                 -> m DeleteIndexFieldResponse
deleteIndexField p1 p2 s =
    send $ (mkDeleteIndexField p1 p2) &~ s

-- | Deletes a suggester. For more information, see Getting Search Suggestions
-- in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DeleteSuggester'
deleteSuggester :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'ds2DomainName'
                -> Text -- ^ 'ds2SuggesterName'
                -> State DeleteSuggester a
                -> m DeleteSuggesterResponse
deleteSuggester p1 p2 s =
    send $ (mkDeleteSuggester p1 p2) &~ s

-- | Gets the analysis schemes configured for a domain. An analysis scheme
-- defines language-specific text processing options for a text field. Can be
-- limited to specific analysis schemes by name. By default, shows all
-- analysis schemes and includes any pending changes to the configuration. Set
-- the Deployed option to true to show the active configuration and exclude
-- pending changes. For more information, see Configuring Analysis Schemes in
-- the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DescribeAnalysisSchemes'
describeAnalysisSchemes :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'das2DomainName'
                        -> State DescribeAnalysisSchemes a
                        -> m DescribeAnalysisSchemesResponse
describeAnalysisSchemes p1 s =
    send $ (mkDescribeAnalysisSchemes p1) &~ s

-- | Gets the availability options configured for a domain. By default, shows
-- the configuration with any pending changes. Set the Deployed option to true
-- to show the active configuration and exclude pending changes. For more
-- information, see Configuring Availability Options in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DescribeAvailabilityOptions'
describeAvailabilityOptions :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'dao2DomainName'
                            -> State DescribeAvailabilityOptions a
                            -> m DescribeAvailabilityOptionsResponse
describeAvailabilityOptions p1 s =
    send $ (mkDescribeAvailabilityOptions p1) &~ s

-- | Gets information about the search domains owned by this account. Can be
-- limited to specific domains. Shows all domains by default. To get the
-- number of searchable documents in a domain, use the console or submit a
-- matchall request to your domain's search endpoint:
-- q=matchall&amp;q.parser=structured&amp;size=0. For more information, see
-- Getting Information about a Search Domain in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DescribeDomains'
describeDomains :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => State DescribeDomains a
                -> m DescribeDomainsResponse
describeDomains s =
    send (mkDescribeDomains &~ s)

-- | Gets the expressions configured for the search domain. Can be limited to
-- specific expressions by name. By default, shows all expressions and
-- includes any pending changes to the configuration. Set the Deployed option
-- to true to show the active configuration and exclude pending changes. For
-- more information, see Configuring Expressions in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DescribeExpressions'
describeExpressions :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'de2DomainName'
                    -> State DescribeExpressions a
                    -> m DescribeExpressionsResponse
describeExpressions p1 s =
    send $ (mkDescribeExpressions p1) &~ s

-- | Gets information about the index fields configured for the search domain.
-- Can be limited to specific fields by name. By default, shows all fields and
-- includes any pending changes to the configuration. Set the Deployed option
-- to true to show the active configuration and exclude pending changes. For
-- more information, see Getting Domain Information in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DescribeIndexFields'
describeIndexFields :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'dif2DomainName'
                    -> State DescribeIndexFields a
                    -> m DescribeIndexFieldsResponse
describeIndexFields p1 s =
    send $ (mkDescribeIndexFields p1) &~ s

-- | Gets the scaling parameters configured for a domain. A domain's scaling
-- parameters specify the desired search instance type and replication count.
-- For more information, see Configuring Scaling Options in the Amazon
-- CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DescribeScalingParameters'
describeScalingParameters :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'dspDomainName'
                          -> State DescribeScalingParameters a
                          -> m DescribeScalingParametersResponse
describeScalingParameters p1 s =
    send $ (mkDescribeScalingParameters p1) &~ s

-- | Gets information about the access policies that control access to the
-- domain's document and search endpoints. By default, shows the configuration
-- with any pending changes. Set the Deployed option to true to show the
-- active configuration and exclude pending changes. For more information, see
-- Configuring Access for a Search Domain in the Amazon CloudSearch Developer
-- Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DescribeServiceAccessPolicies'
describeServiceAccessPolicies :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 , AWSRequest a
                                 )
                              => Text -- ^ 'dsapDomainName'
                              -> State DescribeServiceAccessPolicies a
                              -> m DescribeServiceAccessPoliciesResponse
describeServiceAccessPolicies p1 s =
    send $ (mkDescribeServiceAccessPolicies p1) &~ s

-- | Gets the suggesters configured for a domain. A suggester enables you to
-- display possible matches before users finish typing their queries. Can be
-- limited to specific suggesters by name. By default, shows all suggesters
-- and includes any pending changes to the configuration. Set the Deployed
-- option to true to show the active configuration and exclude pending
-- changes. For more information, see Getting Search Suggestions in the Amazon
-- CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.DescribeSuggesters'
describeSuggesters :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'ds3DomainName'
                   -> State DescribeSuggesters a
                   -> m DescribeSuggestersResponse
describeSuggesters p1 s =
    send $ (mkDescribeSuggesters p1) &~ s

-- | Tells the search domain to start indexing its documents using the latest
-- indexing options. This operation must be invoked to activate options whose
-- OptionStatus is RequiresIndexDocuments.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.IndexDocuments'
indexDocuments :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'idDomainName'
               -> State IndexDocuments a
               -> m IndexDocumentsResponse
indexDocuments p1 s =
    send $ (mkIndexDocuments p1) &~ s

-- | Lists all search domains owned by an account.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.ListDomainNames'
listDomainNames :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => State ListDomainNames a
                -> m ListDomainNamesResponse
listDomainNames s =
    send (mkListDomainNames &~ s)

-- | Configures the availability options for a domain. Enabling the Multi-AZ
-- option expands an Amazon CloudSearch domain to an additional Availability
-- Zone in the same Region to increase fault tolerance in the event of a
-- service disruption. Changes to the Multi-AZ option can take about half an
-- hour to become active. For more information, see Configuring Availability
-- Options in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.UpdateAvailabilityOptions'
updateAvailabilityOptions :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'uaoDomainName'
                          -> Bool -- ^ 'uaoMultiAZ'
                          -> State UpdateAvailabilityOptions a
                          -> m UpdateAvailabilityOptionsResponse
updateAvailabilityOptions p1 p2 s =
    send $ (mkUpdateAvailabilityOptions p1 p2) &~ s

-- | Configures scaling parameters for a domain. A domain's scaling parameters
-- specify the desired search instance type and replication count. Amazon
-- CloudSearch will still automatically scale your domain based on the volume
-- of data and traffic, but not below the desired instance type and
-- replication count. If the Multi-AZ option is enabled, these values control
-- the resources used per Availability Zone. For more information, see
-- Configuring Scaling Options in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.UpdateScalingParameters'
updateScalingParameters :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'uspDomainName'
                        -> ScalingParameters -- ^ 'uspScalingParameters'
                        -> State UpdateScalingParameters a
                        -> m UpdateScalingParametersResponse
updateScalingParameters p1 p2 s =
    send $ (mkUpdateScalingParameters p1 p2) &~ s

-- | Configures the access rules that control access to the domain's document
-- and search endpoints. For more information, see Configuring Access for an
-- Amazon CloudSearch Domain.
--
-- See: 'Network.AWS.CloudSearch.V2013_01_01.UpdateServiceAccessPolicies'
updateServiceAccessPolicies :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'usapDomainName'
                            -> Text -- ^ 'usapAccessPolicies'
                            -> State UpdateServiceAccessPolicies a
                            -> m UpdateServiceAccessPoliciesResponse
updateServiceAccessPolicies p1 p2 s =
    send $ (mkUpdateServiceAccessPolicies p1 p2) &~ s
