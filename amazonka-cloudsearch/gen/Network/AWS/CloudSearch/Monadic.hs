{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.Monadic
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
-- This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.CloudSearch" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.CloudSearch
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.CloudSearch.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Network.AWS.CloudSearch.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
-- @
--
module Network.AWS.CloudSearch.Monadic
    (
    -- * BuildSuggesters
    -- $BuildSuggesters
      buildSuggesters
    , buildSuggestersCatch

    -- * CreateDomain
    -- $CreateDomain
    , createDomain
    , createDomainCatch

    -- * DefineAnalysisScheme
    -- $DefineAnalysisScheme
    , defineAnalysisScheme
    , defineAnalysisSchemeCatch

    -- * DefineExpression
    -- $DefineExpression
    , defineExpression
    , defineExpressionCatch

    -- * DefineIndexField
    -- $DefineIndexField
    , defineIndexField
    , defineIndexFieldCatch

    -- * DefineSuggester
    -- $DefineSuggester
    , defineSuggester
    , defineSuggesterCatch

    -- * DeleteAnalysisScheme
    -- $DeleteAnalysisScheme
    , deleteAnalysisScheme
    , deleteAnalysisSchemeCatch

    -- * DeleteDomain
    -- $DeleteDomain
    , deleteDomain
    , deleteDomainCatch

    -- * DeleteExpression
    -- $DeleteExpression
    , deleteExpression
    , deleteExpressionCatch

    -- * DeleteIndexField
    -- $DeleteIndexField
    , deleteIndexField
    , deleteIndexFieldCatch

    -- * DeleteSuggester
    -- $DeleteSuggester
    , deleteSuggester
    , deleteSuggesterCatch

    -- * DescribeAnalysisSchemes
    -- $DescribeAnalysisSchemes
    , describeAnalysisSchemes
    , describeAnalysisSchemesCatch

    -- * DescribeAvailabilityOptions
    -- $DescribeAvailabilityOptions
    , describeAvailabilityOptions
    , describeAvailabilityOptionsCatch

    -- * DescribeDomains
    -- $DescribeDomains
    , describeDomains
    , describeDomainsCatch

    -- * DescribeExpressions
    -- $DescribeExpressions
    , describeExpressions
    , describeExpressionsCatch

    -- * DescribeIndexFields
    -- $DescribeIndexFields
    , describeIndexFields
    , describeIndexFieldsCatch

    -- * DescribeScalingParameters
    -- $DescribeScalingParameters
    , describeScalingParameters
    , describeScalingParametersCatch

    -- * DescribeServiceAccessPolicies
    -- $DescribeServiceAccessPolicies
    , describeServiceAccessPolicies
    , describeServiceAccessPoliciesCatch

    -- * DescribeSuggesters
    -- $DescribeSuggesters
    , describeSuggesters
    , describeSuggestersCatch

    -- * IndexDocuments
    -- $IndexDocuments
    , indexDocuments
    , indexDocumentsCatch

    -- * ListDomainNames
    -- $ListDomainNames
    , listDomainNames
    , listDomainNamesCatch

    -- * UpdateAvailabilityOptions
    -- $UpdateAvailabilityOptions
    , updateAvailabilityOptions
    , updateAvailabilityOptionsCatch

    -- * UpdateScalingParameters
    -- $UpdateScalingParameters
    , updateScalingParameters
    , updateScalingParametersCatch

    -- * UpdateServiceAccessPolicies
    -- $UpdateServiceAccessPolicies
    , updateServiceAccessPolicies
    , updateServiceAccessPoliciesCatch

    -- * Re-exported
    , module Network.AWS.CloudSearch

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.CloudSearch

type ServiceEr = Er CloudSearch

-- $BuildSuggesters
-- Indexes the search suggestions.
--
-- See: 'Network.AWS.CloudSearch'

buildSuggesters :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'bsDomainName'
    -> State BuildSuggesters a
    -> m BuildSuggestersResponse
buildSuggesters p1 s =
    send $ (mkBuildSuggesters p1) &~ s

buildSuggestersCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'bsDomainName'
    -> State BuildSuggesters a
    -> m (Either ServiceEr BuildSuggestersResponse)
buildSuggestersCatch p1 s =
    sendCatch $ (mkBuildSuggesters p1) &~ s

-- $CreateDomain
-- Creates a new search domain. For more information, see Creating a Search
-- Domain in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

createDomain :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'cdDomainName'
    -> State CreateDomain a
    -> m CreateDomainResponse
createDomain p1 s =
    send $ (mkCreateDomain p1) &~ s

createDomainCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'cdDomainName'
    -> State CreateDomain a
    -> m (Either ServiceEr CreateDomainResponse)
createDomainCatch p1 s =
    sendCatch $ (mkCreateDomain p1) &~ s

-- $DefineAnalysisScheme
-- Configures an analysis scheme that can be applied to a text or text-array
-- field to define language-specific text processing options. For more
-- information, see Configuring Analysis Schemes in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

defineAnalysisScheme :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'dasDomainName'
    -> AnalysisScheme -- ^ 'dasAnalysisScheme'
    -> State DefineAnalysisScheme a
    -> m DefineAnalysisSchemeResponse
defineAnalysisScheme p1 p2 s =
    send $ (mkDefineAnalysisScheme p1 p2) &~ s

defineAnalysisSchemeCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dasDomainName'
    -> AnalysisScheme -- ^ 'dasAnalysisScheme'
    -> State DefineAnalysisScheme a
    -> m (Either ServiceEr DefineAnalysisSchemeResponse)
defineAnalysisSchemeCatch p1 p2 s =
    sendCatch $ (mkDefineAnalysisScheme p1 p2) &~ s

-- $DefineExpression
-- Configures an Expression for the search domain. Used to create new
-- expressions and modify existing ones. If the expression exists, the new
-- configuration replaces the old one. For more information, see Configuring
-- Expressions in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

defineExpression :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'deDomainName'
    -> Expression -- ^ 'deExpression'
    -> State DefineExpression a
    -> m DefineExpressionResponse
defineExpression p1 p2 s =
    send $ (mkDefineExpression p1 p2) &~ s

defineExpressionCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'deDomainName'
    -> Expression -- ^ 'deExpression'
    -> State DefineExpression a
    -> m (Either ServiceEr DefineExpressionResponse)
defineExpressionCatch p1 p2 s =
    sendCatch $ (mkDefineExpression p1 p2) &~ s

-- $DefineIndexField
-- Configures an IndexField for the search domain. Used to create new fields
-- and modify existing ones. You must specify the name of the domain you are
-- configuring and an index field configuration. The index field configuration
-- specifies a unique name, the index field type, and the options you want to
-- configure for the field. The options you can specify depend on the
-- IndexFieldType. If the field exists, the new configuration replaces the old
-- one. For more information, see Configuring Index Fields in the Amazon
-- CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

defineIndexField :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'difDomainName'
    -> IndexField -- ^ 'difIndexField'
    -> State DefineIndexField a
    -> m DefineIndexFieldResponse
defineIndexField p1 p2 s =
    send $ (mkDefineIndexField p1 p2) &~ s

defineIndexFieldCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'difDomainName'
    -> IndexField -- ^ 'difIndexField'
    -> State DefineIndexField a
    -> m (Either ServiceEr DefineIndexFieldResponse)
defineIndexFieldCatch p1 p2 s =
    sendCatch $ (mkDefineIndexField p1 p2) &~ s

-- $DefineSuggester
-- Configures a suggester for a domain. A suggester enables you to display
-- possible matches before users finish typing their queries. When you
-- configure a suggester, you must specify the name of the text field you want
-- to search for possible matches and a unique name for the suggester. For
-- more information, see Getting Search Suggestions in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

defineSuggester :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'ds1DomainName'
    -> Suggester -- ^ 'ds1Suggester'
    -> State DefineSuggester a
    -> m DefineSuggesterResponse
defineSuggester p1 p2 s =
    send $ (mkDefineSuggester p1 p2) &~ s

defineSuggesterCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'ds1DomainName'
    -> Suggester -- ^ 'ds1Suggester'
    -> State DefineSuggester a
    -> m (Either ServiceEr DefineSuggesterResponse)
defineSuggesterCatch p1 p2 s =
    sendCatch $ (mkDefineSuggester p1 p2) &~ s

-- $DeleteAnalysisScheme
-- Deletes an analysis scheme. For more information, see Configuring Analysis
-- Schemes in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

deleteAnalysisScheme :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'das1DomainName'
    -> Text -- ^ 'das1AnalysisSchemeName'
    -> State DeleteAnalysisScheme a
    -> m DeleteAnalysisSchemeResponse
deleteAnalysisScheme p1 p2 s =
    send $ (mkDeleteAnalysisScheme p1 p2) &~ s

deleteAnalysisSchemeCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'das1DomainName'
    -> Text -- ^ 'das1AnalysisSchemeName'
    -> State DeleteAnalysisScheme a
    -> m (Either ServiceEr DeleteAnalysisSchemeResponse)
deleteAnalysisSchemeCatch p1 p2 s =
    sendCatch $ (mkDeleteAnalysisScheme p1 p2) &~ s

-- $DeleteDomain
-- Permanently deletes a search domain and all of its data. Once a domain has
-- been deleted, it cannot be recovered. For more information, see Deleting a
-- Search Domain in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

deleteDomain :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'ddDomainName'
    -> State DeleteDomain a
    -> m DeleteDomainResponse
deleteDomain p1 s =
    send $ (mkDeleteDomain p1) &~ s

deleteDomainCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'ddDomainName'
    -> State DeleteDomain a
    -> m (Either ServiceEr DeleteDomainResponse)
deleteDomainCatch p1 s =
    sendCatch $ (mkDeleteDomain p1) &~ s

-- $DeleteExpression
-- Removes an Expression from the search domain. For more information, see
-- Configuring Expressions in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

deleteExpression :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'de1DomainName'
    -> Text -- ^ 'de1ExpressionName'
    -> State DeleteExpression a
    -> m DeleteExpressionResponse
deleteExpression p1 p2 s =
    send $ (mkDeleteExpression p1 p2) &~ s

deleteExpressionCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'de1DomainName'
    -> Text -- ^ 'de1ExpressionName'
    -> State DeleteExpression a
    -> m (Either ServiceEr DeleteExpressionResponse)
deleteExpressionCatch p1 p2 s =
    sendCatch $ (mkDeleteExpression p1 p2) &~ s

-- $DeleteIndexField
-- Removes an IndexField from the search domain. For more information, see
-- Configuring Index Fields in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

deleteIndexField :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'dif1DomainName'
    -> Text -- ^ 'dif1IndexFieldName'
    -> State DeleteIndexField a
    -> m DeleteIndexFieldResponse
deleteIndexField p1 p2 s =
    send $ (mkDeleteIndexField p1 p2) &~ s

deleteIndexFieldCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dif1DomainName'
    -> Text -- ^ 'dif1IndexFieldName'
    -> State DeleteIndexField a
    -> m (Either ServiceEr DeleteIndexFieldResponse)
deleteIndexFieldCatch p1 p2 s =
    sendCatch $ (mkDeleteIndexField p1 p2) &~ s

-- $DeleteSuggester
-- Deletes a suggester. For more information, see Getting Search Suggestions
-- in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

deleteSuggester :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'ds2DomainName'
    -> Text -- ^ 'ds2SuggesterName'
    -> State DeleteSuggester a
    -> m DeleteSuggesterResponse
deleteSuggester p1 p2 s =
    send $ (mkDeleteSuggester p1 p2) &~ s

deleteSuggesterCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'ds2DomainName'
    -> Text -- ^ 'ds2SuggesterName'
    -> State DeleteSuggester a
    -> m (Either ServiceEr DeleteSuggesterResponse)
deleteSuggesterCatch p1 p2 s =
    sendCatch $ (mkDeleteSuggester p1 p2) &~ s

-- $DescribeAnalysisSchemes
-- Gets the analysis schemes configured for a domain. An analysis scheme
-- defines language-specific text processing options for a text field. Can be
-- limited to specific analysis schemes by name. By default, shows all
-- analysis schemes and includes any pending changes to the configuration. Set
-- the Deployed option to true to show the active configuration and exclude
-- pending changes. For more information, see Configuring Analysis Schemes in
-- the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

describeAnalysisSchemes :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'das2DomainName'
    -> State DescribeAnalysisSchemes a
    -> m DescribeAnalysisSchemesResponse
describeAnalysisSchemes p1 s =
    send $ (mkDescribeAnalysisSchemes p1) &~ s

describeAnalysisSchemesCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'das2DomainName'
    -> State DescribeAnalysisSchemes a
    -> m (Either ServiceEr DescribeAnalysisSchemesResponse)
describeAnalysisSchemesCatch p1 s =
    sendCatch $ (mkDescribeAnalysisSchemes p1) &~ s

-- $DescribeAvailabilityOptions
-- Gets the availability options configured for a domain. By default, shows
-- the configuration with any pending changes. Set the Deployed option to true
-- to show the active configuration and exclude pending changes. For more
-- information, see Configuring Availability Options in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

describeAvailabilityOptions :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'dao2DomainName'
    -> State DescribeAvailabilityOptions a
    -> m DescribeAvailabilityOptionsResponse
describeAvailabilityOptions p1 s =
    send $ (mkDescribeAvailabilityOptions p1) &~ s

describeAvailabilityOptionsCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'dao2DomainName'
    -> State DescribeAvailabilityOptions a
    -> m (Either ServiceEr DescribeAvailabilityOptionsResponse)
describeAvailabilityOptionsCatch p1 s =
    sendCatch $ (mkDescribeAvailabilityOptions p1) &~ s

-- $DescribeDomains
-- Gets information about the search domains owned by this account. Can be
-- limited to specific domains. Shows all domains by default. To get the
-- number of searchable documents in a domain, use the console or submit a
-- matchall request to your domain's search endpoint:
-- q=matchall&amp;q.parser=structured&amp;size=0. For more information, see
-- Getting Information about a Search Domain in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

describeDomains :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => State DescribeDomains a
    -> m DescribeDomainsResponse
describeDomains s =
    send (mkDescribeDomains &~ s)

describeDomainsCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => State DescribeDomains a
    -> m (Either ServiceEr DescribeDomainsResponse)
describeDomainsCatch s =
    sendCatch (mkDescribeDomains &~ s)

-- $DescribeExpressions
-- Gets the expressions configured for the search domain. Can be limited to
-- specific expressions by name. By default, shows all expressions and
-- includes any pending changes to the configuration. Set the Deployed option
-- to true to show the active configuration and exclude pending changes. For
-- more information, see Configuring Expressions in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

describeExpressions :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'de2DomainName'
    -> State DescribeExpressions a
    -> m DescribeExpressionsResponse
describeExpressions p1 s =
    send $ (mkDescribeExpressions p1) &~ s

describeExpressionsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'de2DomainName'
    -> State DescribeExpressions a
    -> m (Either ServiceEr DescribeExpressionsResponse)
describeExpressionsCatch p1 s =
    sendCatch $ (mkDescribeExpressions p1) &~ s

-- $DescribeIndexFields
-- Gets information about the index fields configured for the search domain.
-- Can be limited to specific fields by name. By default, shows all fields and
-- includes any pending changes to the configuration. Set the Deployed option
-- to true to show the active configuration and exclude pending changes. For
-- more information, see Getting Domain Information in the Amazon CloudSearch
-- Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

describeIndexFields :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dif2DomainName'
    -> State DescribeIndexFields a
    -> m DescribeIndexFieldsResponse
describeIndexFields p1 s =
    send $ (mkDescribeIndexFields p1) &~ s

describeIndexFieldsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'dif2DomainName'
    -> State DescribeIndexFields a
    -> m (Either ServiceEr DescribeIndexFieldsResponse)
describeIndexFieldsCatch p1 s =
    sendCatch $ (mkDescribeIndexFields p1) &~ s

-- $DescribeScalingParameters
-- Gets the scaling parameters configured for a domain. A domain's scaling
-- parameters specify the desired search instance type and replication count.
-- For more information, see Configuring Scaling Options in the Amazon
-- CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

describeScalingParameters :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dspDomainName'
    -> State DescribeScalingParameters a
    -> m DescribeScalingParametersResponse
describeScalingParameters p1 s =
    send $ (mkDescribeScalingParameters p1) &~ s

describeScalingParametersCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'dspDomainName'
    -> State DescribeScalingParameters a
    -> m (Either ServiceEr DescribeScalingParametersResponse)
describeScalingParametersCatch p1 s =
    sendCatch $ (mkDescribeScalingParameters p1) &~ s

-- $DescribeServiceAccessPolicies
-- Gets information about the access policies that control access to the
-- domain's document and search endpoints. By default, shows the configuration
-- with any pending changes. Set the Deployed option to true to show the
-- active configuration and exclude pending changes. For more information, see
-- Configuring Access for a Search Domain in the Amazon CloudSearch Developer
-- Guide.
--
-- See: 'Network.AWS.CloudSearch'

describeServiceAccessPolicies :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'dsapDomainName'
    -> State DescribeServiceAccessPolicies a
    -> m DescribeServiceAccessPoliciesResponse
describeServiceAccessPolicies p1 s =
    send $ (mkDescribeServiceAccessPolicies p1) &~ s

describeServiceAccessPoliciesCatch :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadReader Env m
                                      )
    => Text -- ^ 'dsapDomainName'
    -> State DescribeServiceAccessPolicies a
    -> m (Either ServiceEr DescribeServiceAccessPoliciesResponse)
describeServiceAccessPoliciesCatch p1 s =
    sendCatch $ (mkDescribeServiceAccessPolicies p1) &~ s

-- $DescribeSuggesters
-- Gets the suggesters configured for a domain. A suggester enables you to
-- display possible matches before users finish typing their queries. Can be
-- limited to specific suggesters by name. By default, shows all suggesters
-- and includes any pending changes to the configuration. Set the Deployed
-- option to true to show the active configuration and exclude pending
-- changes. For more information, see Getting Search Suggestions in the Amazon
-- CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

describeSuggesters :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'ds3DomainName'
    -> State DescribeSuggesters a
    -> m DescribeSuggestersResponse
describeSuggesters p1 s =
    send $ (mkDescribeSuggesters p1) &~ s

describeSuggestersCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'ds3DomainName'
    -> State DescribeSuggesters a
    -> m (Either ServiceEr DescribeSuggestersResponse)
describeSuggestersCatch p1 s =
    sendCatch $ (mkDescribeSuggesters p1) &~ s

-- $IndexDocuments
-- Tells the search domain to start indexing its documents using the latest
-- indexing options. This operation must be invoked to activate options whose
-- OptionStatus is RequiresIndexDocuments.
--
-- See: 'Network.AWS.CloudSearch'

indexDocuments :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'idDomainName'
    -> State IndexDocuments a
    -> m IndexDocumentsResponse
indexDocuments p1 s =
    send $ (mkIndexDocuments p1) &~ s

indexDocumentsCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'idDomainName'
    -> State IndexDocuments a
    -> m (Either ServiceEr IndexDocumentsResponse)
indexDocumentsCatch p1 s =
    sendCatch $ (mkIndexDocuments p1) &~ s

-- $ListDomainNames
-- Lists all search domains owned by an account.
--
-- See: 'Network.AWS.CloudSearch'

listDomainNames :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => State ListDomainNames a
    -> m ListDomainNamesResponse
listDomainNames s =
    send (mkListDomainNames &~ s)

listDomainNamesCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => State ListDomainNames a
    -> m (Either ServiceEr ListDomainNamesResponse)
listDomainNamesCatch s =
    sendCatch (mkListDomainNames &~ s)

-- $UpdateAvailabilityOptions
-- Configures the availability options for a domain. Enabling the Multi-AZ
-- option expands an Amazon CloudSearch domain to an additional Availability
-- Zone in the same Region to increase fault tolerance in the event of a
-- service disruption. Changes to the Multi-AZ option can take about half an
-- hour to become active. For more information, see Configuring Availability
-- Options in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

updateAvailabilityOptions :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'uaoDomainName'
    -> Bool -- ^ 'uaoMultiAZ'
    -> State UpdateAvailabilityOptions a
    -> m UpdateAvailabilityOptionsResponse
updateAvailabilityOptions p1 p2 s =
    send $ (mkUpdateAvailabilityOptions p1 p2) &~ s

updateAvailabilityOptionsCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'uaoDomainName'
    -> Bool -- ^ 'uaoMultiAZ'
    -> State UpdateAvailabilityOptions a
    -> m (Either ServiceEr UpdateAvailabilityOptionsResponse)
updateAvailabilityOptionsCatch p1 p2 s =
    sendCatch $ (mkUpdateAvailabilityOptions p1 p2) &~ s

-- $UpdateScalingParameters
-- Configures scaling parameters for a domain. A domain's scaling parameters
-- specify the desired search instance type and replication count. Amazon
-- CloudSearch will still automatically scale your domain based on the volume
-- of data and traffic, but not below the desired instance type and
-- replication count. If the Multi-AZ option is enabled, these values control
-- the resources used per Availability Zone. For more information, see
-- Configuring Scaling Options in the Amazon CloudSearch Developer Guide.
--
-- See: 'Network.AWS.CloudSearch'

updateScalingParameters :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'uspDomainName'
    -> ScalingParameters -- ^ 'uspScalingParameters'
    -> State UpdateScalingParameters a
    -> m UpdateScalingParametersResponse
updateScalingParameters p1 p2 s =
    send $ (mkUpdateScalingParameters p1 p2) &~ s

updateScalingParametersCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'uspDomainName'
    -> ScalingParameters -- ^ 'uspScalingParameters'
    -> State UpdateScalingParameters a
    -> m (Either ServiceEr UpdateScalingParametersResponse)
updateScalingParametersCatch p1 p2 s =
    sendCatch $ (mkUpdateScalingParameters p1 p2) &~ s

-- $UpdateServiceAccessPolicies
-- Configures the access rules that control access to the domain's document
-- and search endpoints. For more information, see Configuring Access for an
-- Amazon CloudSearch Domain.
--
-- See: 'Network.AWS.CloudSearch'

updateServiceAccessPolicies :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'usapDomainName'
    -> Text -- ^ 'usapAccessPolicies'
    -> State UpdateServiceAccessPolicies a
    -> m UpdateServiceAccessPoliciesResponse
updateServiceAccessPolicies p1 p2 s =
    send $ (mkUpdateServiceAccessPolicies p1 p2) &~ s

updateServiceAccessPoliciesCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'usapDomainName'
    -> Text -- ^ 'usapAccessPolicies'
    -> State UpdateServiceAccessPolicies a
    -> m (Either ServiceEr UpdateServiceAccessPoliciesResponse)
updateServiceAccessPoliciesCatch p1 p2 s =
    sendCatch $ (mkUpdateServiceAccessPolicies p1 p2) &~ s
